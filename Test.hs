{-# LANGUAGE StandaloneDeriving #-}
module Main(main) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import HAppS.Server
import HAppS.Server.JSON
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Exception
import Char
import Database.SQLite
import Data.IORef
import Random
import System.IO.Unsafe
import GHC.Int
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim hiding (getInput)
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Char

import Debug.Trace

deriving instance Show JSON

instance ToJSON a => ToJSON [a] where
    toJSON xs = JList $ map toJSON xs

second :: (a -> b) -> (c, a) -> (c, b)
second f (a, b) = (a, f b)

stringToBSL :: String -> BSL.ByteString
stringToBSL = BSL.pack . (map (fromInteger.toInteger.ord))

bslToString :: BSL.ByteString -> String
bslToString = map (chr . fromInteger . toInteger) . BSL.unpack

safeBslToString :: BSL.ByteString -> String
safeBslToString bsl =
    if BSL.length bsl > 10000
    then error "unpack very large BSL"
    else bslToString bsl

jsonResponse :: JSON -> BSL.ByteString
jsonResponse = stringToBSL. jsonToString

forceMaybe :: Maybe a -> a
forceMaybe = maybe (error "whoops forced Nothing") id

forceLookup :: Eq a => a -> [(a, b)] -> b
forceLookup key table = forceMaybe $ lookup key table

lookupDefault :: Eq a => a -> [(a,b)] -> b -> b
lookupDefault key table def =
    maybe def id (lookup key table)

filterCharacterUname :: Char -> Char
filterCharacterUname x | (isAlpha x || isDigit x || x == '_') = x
                       | otherwise = '_'

jsonError :: String -> JSON
jsonError msg = JObj [("result", JString "error"),
                      ("error", JString msg)]

simpleError :: (Monad m) => String -> m Response
simpleError msg = return $ resultBS 200 $ jsonResponse $ jsonError msg

simpleSuccess :: (Monad m, ToJSON a) => a -> m Response
simpleSuccess what = return $ resultBS 200 $ jsonResponse $
                     JObj [("result", JString "okay"),
                           ("data", toJSON what)]

trivialSuccess :: Monad m => m Response
trivialSuccess = return $ resultBS 200 $ jsonResponse $
                 JObj [("result", JString "okay")]

findBalance :: SQLiteHandle -> String -> IO (Either String Double)
findBalance db uname =
    do charges <- execParamStatement db
                  "SELECT amount FROM charges WHERE user = :user"
                  [(":user", Text uname)]
       case charges of
         Left msg -> return (Left msg)
         Right r ->
             let unwrap :: Row Value -> Double
                 unwrap xs =
                     let (Double r) = snd $ head xs
                     in r
             in
               return $ Right $ sum $ map (unwrap) $ concat r

handle_get_user_list :: (MonadIO m) => SQLiteHandle -> m Response
handle_get_user_list db =
    let doOneEntry :: Row String -> IO (Either String JSON)
        doOneEntry r =
            let uname = forceLookup "username" r
            in
              do balance <- findBalance db uname
                 case balance of
                   Left msg -> return (Left msg)
                   Right bal ->
                       return $ Right $ JObj [("uname", JString $ forceLookup "username" r),
                                              ("balance", JFloat $ realToFrac bal)]
    in
    do res <- liftIO $ execStatement db
              "select \"username\" from users;"
       case res of
         Left msg ->
             simpleError msg
         Right r ->
             do resData <- liftIO $ mapM doOneEntry $ concat r
                let deEither [] = Right []
                    deEither ((Left msg):_) = Left msg
                    deEither ((Right x):xs) =
                        case deEither xs of
                          Left msg -> Left msg
                          Right xs' -> Right (x:xs')
                case deEither resData of
                    Left msg -> simpleError msg
                    Right r -> simpleSuccess $ JList r

getInput :: Request -> String -> String
getInput rq key =
    let res = inputValue $ forceLookup key $ rqInputs rq
    in if BSL.length res > 10000 then "_too_long_"
       else bslToString res

handle_remove_user :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_remove_user db rq =
    let uname = getInput rq "username"
    in do is_admin <- isAdmin $ getInput rq "cookie"
          if not is_admin then simpleError "Need to be admin to remove users"
           else
           do
            res <- liftIO $ execParamStatement_ db
                   "delete from users where username=:user;"
                   [(":user", Text uname)]
            return $ resultBS 200 $ jsonResponse $
                 case res of
                   Just msg -> jsonError msg
                   Nothing -> JObj [("result", JString "okay")]

jsonParser :: GenParser Char () JSON
jsonParser =
    jsonValue
    where
      tokenParser = makeTokenParser haskellDef
      jsonValue =
          skipSpace >> 
             (jsonString <|> jsonNumber <|> jsonObject <|> jsonArray
              <|> jsonTrue <|> jsonFalse <|> jsonNull)
      skipSpace = many $ oneOf " \r\t\n\v"
      jsonString = do s <- stringLiteral tokenParser
                      return $ JString s
      jsonNumber :: GenParser Char () JSON
      jsonNumber = do s <- naturalOrFloat tokenParser
                      return $ case s of
                        Left i -> jInt i
                        Right f -> JFloat $ realToFrac f
      jsonObject :: GenParser Char () JSON
      jsonObject = do char '{'
                      body <- sepBy (do skipSpace
                                        n <- stringLiteral tokenParser
                                        skipSpace
                                        char ':'
                                        skipSpace
                                        v <- jsonValue
                                        skipSpace
                                        return (n,v)) (char ',')
                      skipSpace
                      char '}'
                      return $ JObj body
      jsonArray = do char '['
                     skipSpace
                     body <- sepBy jsonValue (skipSpace >> char ',')
                     skipSpace
                     char ']'
                     return $ JList body
      jsonTrue = do string "true"
                    return $ JBool True
      jsonFalse = do string "false"
                     return $ JBool False
      jsonNull = do string "null"
                    return JNull

parseJSON :: String -> Maybe JSON
parseJSON what =
    case parse jsonParser "" what of
      Left msg -> trace ("error parsing json " ++ what ++ " -> " ++ (show msg)) Nothing
      Right x -> Just x

data ChargeRecord = ChargeRecord { cr_user :: String,
                                   cr_amount :: Double } deriving Show

parseJsonChargeRecord :: JSON -> ChargeRecord
parseJsonChargeRecord (JObj fields) =
    let u = forceLookup "user" fields
        a = forceLookup "charge" fields
    in case (a, u) of
         (JString a', JString u') ->
             ChargeRecord { cr_user = u', cr_amount = read a' }
         _ -> error "broken JSON charge record"
parseJsonChargeRecord _ = error "expected JSON object encoding of charge record"

parseJsonChargeRecords :: JSON -> [ChargeRecord]
parseJsonChargeRecords (JList i) = map parseJsonChargeRecord i
parseJsonChargeRecords _ = error "expected JSON list of charge records"

sqlTransaction :: SQLiteHandle -> IO x -> IO x
sqlTransaction db body =
    do r <- execStatement_ db "BEGIN TRANSACTION;"
       case r of
         Just msg -> error ("failed to begin transaction: " ++ msg)
         Nothing ->
             do result <- Control.Exception.onException body (execStatement_ db "ROLLBACK;")
                r2 <- execStatement_ db "COMMIT;"
                case r2 of
                  Just x -> trace ("failed to commit: " ++ x) $ sqlTransaction db body
                  Nothing -> return result

deMaybe :: [Maybe x] -> [x]
deMaybe [] = []
deMaybe (Just x : xs) = x:(deMaybe xs)
deMaybe (Nothing:xs) = deMaybe xs

maybeErrListToMaybeErr :: [Maybe String] -> Maybe String
maybeErrListToMaybeErr errs =
    let errs' = deMaybe errs in
    case errs' of
      [] -> Nothing
      _ -> Just $ foldr (\a b -> a ++ ", " ++ b) "" errs'

validate_date :: String -> String
validate_date what =
    let [year', month', day'] = split_on '-' what
        split_on :: Eq a => a -> [a] -> [[a]]
        split_on key string =
            foldr (\char accumulated ->
                       case accumulated of
                         [] -> if char == key
                               then [[]]
                               else [[char]]
                         (acc1:accs) ->
                             if char == key
                             then []:accumulated
                             else (char:acc1):accs) [] string
        year = read year'
        month = read month'
        day = read day'
    in if or [length year' /= 4, length month' /= 2, length day' /= 2,
              year < 2008, year > 2020, month < 1, month > 12,
              day < 1, day > 30]
       then error $ "Bad date " ++ what
       else
           {- 30 -> 9, 4, 6, 11
              feb -> 2 -}
           if day > (case month of
                       x | x `elem` [1,3,5,7,8,9,10,12] -> 31
                         | x `elem` [4,6,9,11] -> 30
                         | x == 2 ->
                             if year `mod` 400 == 0
                             then 29
                             else if year `mod` 100 == 0
                                  then 28
                                  else if year `mod` 4 == 0
                                       then 29
                                       else 28)
           then error $ "Bad day of month " ++ what
           else what

handle_add_bill :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_add_bill db rq =
    let description = getInput rq "description"
        date = validate_date $ getInput rq "date"
        to_pay =
            parseJsonChargeRecords $ forceMaybe $ parseJSON $ getInput rq "to_pay"
        to_receive =
            parseJsonChargeRecords $ forceMaybe $ parseJSON $ getInput rq "to_receive"
        tot_pay = sum $ map cr_amount to_pay
        tot_recv = sum $ map cr_amount to_receive
        insertPay :: Integer -> ChargeRecord -> IO (Maybe String)
        insertPay bill cr =
            execParamStatement_ db
                 "INSERT into charges (\"bill\", \"user\", \"amount\") values (:bill, :user, :amount);"
                 [(":bill", Int $ fromInteger bill),
                  (":user", Text $ cr_user cr),
                  (":amount", Double $ cr_amount cr)]
        insertReceive :: Integer -> ChargeRecord -> IO (Maybe String)
        insertReceive bill cr =
            execParamStatement_ db
                 "INSERT into charges (\"bill\", \"user\", \"amount\") values (:bill, :user, :amount);"
                 [(":bill", Int $ fromInteger bill),
                  (":user", Text $ cr_user cr),
                  (":amount", Double (- (cr_amount cr)))]

    in if abs (tot_pay - tot_recv) > 0.01
       then return $ resultBS 200 $ jsonResponse $ jsonError $ "Total payment " ++ (show tot_pay) ++ " doesn't match total receipt " ++ (show tot_recv)
       else
           do my_uname <- cookieUname $ getInput rq "cookie"
              res <- liftIO $ sqlTransaction db $
                     do r1 <- execParamStatement_ db
                              "INSERT into bills (\"date\", \"description\", \"owner\") values (:date, :description, :owner);"
                              [(":date", Text date),
                               (":description", Text description),
                               (":owner", Text my_uname)]
                        case r1 of
                          Just err -> return $ Just ("creating bill owned by " ++ my_uname ++ ": " ++ err)
                          Nothing ->
                              do bill <- getLastRowID db
                                 r2 <- mapM (insertPay bill) to_pay
                                 r3 <- mapM (insertReceive bill) to_receive
                                 return $ maybeErrListToMaybeErr (r2 ++ r3)
              return $ resultBS 200 $ jsonResponse $
                     case res of
                       Nothing -> JObj [("result", JString "okay")]
                       Just err -> jsonError err

data BillEntry = BillEntry { be_ident :: Int,
                             be_date :: String,
                             be_description :: String,
                             be_owner :: String,
                             be_charges :: [(Int, String, Double)],
                             be_attachments :: [(Int, String)] } deriving Show

instance ToJSON BillEntry where
    toJSON be = JObj [("ident", JInt $ be_ident be),
                      ("date", JString $ be_date be),
                      ("description", JString $be_description be),
                      ("owner", JString $ be_owner be),
                      ("charges", JList [JObj [("ident", JInt ident),
                                               ("charge", (JFloat $ realToFrac charge)),
                                               ("uname", (JString user))]
                                               |
                                               (ident, user, charge) <- be_charges be]),
                      ("attachments", JList [JObj [("ident", JInt ident),
                                                   ("name", JString name)]
                                             | (ident, name) <- be_attachments be])]

maximumUploadSize :: Int64
maximumUploadSize = 500000

handle_remove_bill_attachment :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_remove_bill_attachment db rq =
    let attachIdent = read $ getInput rq "id"
    in
    do my_uname <- whoAmI rq
       r <- liftIO $ execParamStatement db
            "SELECT bills.owner FROM bills,bill_attachments WHERE bill_attachments.attach_ident = :ident AND bills.billident = bill_attachments.bill_ident;"
            [(":ident", Int attachIdent)]
       case r of
         Left msg -> simpleError msg
         Right x ->
             let (Text owner_uname) = forceLookup "owner" $ head $ concat x
             in if owner_uname /= my_uname
                then simpleError "can't remove attachments you don't own"
                else
                    do r <- liftIO $ execParamStatement_ db
                            "DELETE FROM bill_attachments WHERE attach_ident = :ident;"
                            [(":ident", Int attachIdent)]
                       case r of
                         Nothing -> trivialSuccess
                         Just msg -> simpleError msg
               

handle_fetch_attachment :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_fetch_attachment db rq =
    let ident = read $ getInput rq "id"
    in do r <- liftIO $ execParamStatement db
               "SELECT content FROM bill_attachments WHERE attach_ident = :ident;"
               [(":ident", Int ident)]
          case r of
            Left msg -> simpleError msg
            Right x ->
                let (Blob content) = forceLookup "content" $ head $ concat x
                in return $ setHeader "content-type" "application/octet-stream" $ resultBS 200 $ BSL.fromChunks [content]

handle_attach_file :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_attach_file db rq =
    let body = bodyInput rq
        file = forceLookup "file_to_attach" body
        file_body = inputValue file
        file_name = forceMaybe $ inputFilename file
        cookie = safeBslToString $ inputValue $ forceLookup "cookie" body
        billIdent :: Int64
        billIdent = read $ safeBslToString $ inputValue $ forceLookup "bill" body
        referer = map (chr . fromInteger . toInteger) $ BS.unpack $ forceMaybe $ getHeader "Referer" rq
    in
      if BSL.length file_body > maximumUploadSize
      then simpleError "File is too big to attach"
      else
          do my_uname <- cookieUname cookie
             billOwner <- liftIO $ getBillOwner db billIdent
             if billOwner /= my_uname
              then simpleError "Can't attach files to someone else's bills"
              else
                  do r <- liftIO $ execParamStatement_ db
                          "INSERT INTO bill_attachments (\"bill_ident\", \"content\", \"filename\") VALUES (:bill_ident, :content, :filename);"
                          [(":bill_ident", Int billIdent),
                           (":content", Blob $ BS.concat $ BSL.toChunks file_body),
                           (":filename", Text file_name)]
                     case r of
                       Just msg -> simpleError msg
                       Nothing ->
                           do attachIdent <- liftIO $ getLastRowID db
                              return $ addHeader "Location" referer $
                                     addHeader "X-AttachmentId" (show attachIdent) $
                                     resultBS 303 $ stringToBSL $
                                              "<html><body><a href=\"" ++ referer ++ "\">Redirecting...</a></body></html>"

handle_old_bills :: (MonadIO m) => SQLiteHandle -> m Response
handle_old_bills db =
    do bills' <- liftIO $ execStatement db "SELECT * FROM bills ORDER BY date DESC;"
       case bills' of
         Left msg -> simpleError msg
         Right bills ->
             do charges' <- liftIO $ execStatement db "SELECT * FROM CHARGES;"
                case charges' of
                  Left msg -> simpleError msg
                  Right charges ->
                      let 
                          formatCharge charge =
                              (fromInteger $ toInteger ident, bill, user, amount) where
                                  (Int ident) = forceLookup "chargeident" charge
                                  (Int bill) = forceLookup "bill" charge
                                  (Text user) = forceLookup "user" charge
                                  (Double amount) = forceLookup "amount" charge
                          formatBill bill =
                              (ident, date, description, owner) where
                                  (Int ident) = forceLookup "billident" bill
                                  (Text date) = forceLookup "date" bill
                                  (Text description) = forceLookup "description" bill
                                  owner =
                                      case forceLookup "owner" bill of
                                        Text o -> o
                                        Null -> ""
                          formatAttachment att =
                               (fromInteger $ toInteger ident, fname) where
                                   (Int ident) = forceLookup "attach_ident" att
                                   (Text fname) = forceLookup "filename" att
                          formattedCharges = map formatCharge $ concat charges
                          formattedBills = map formatBill $ concat bills
                          attachmentsForBill bill =
                              do r <- execParamStatement db "SELECT attach_ident, filename FROM bill_attachments WHERE bill_ident = :bill"
                                      [(":bill", Int bill)]
                                 case r of
                                   Left msg -> error "Getting attachments for bill"
                                   Right x ->
                                       return $ map formatAttachment $ concat x
                          chargesForBill bill =
                               [(ident, user, realToFrac amount) |
                                (ident, bill', user, amount) <- formattedCharges, bill' == bill]
                          mkBillEntry (ident, date, description, owner) =
                               do attaches <- liftIO $ attachmentsForBill ident
                                  return $ BillEntry { be_ident = fromInteger $ toInteger ident,
                                                       be_date = date,
                                                       be_description = description,
                                                       be_owner = owner,
                                                       be_charges = chargesForBill ident,
                                                       be_attachments = attaches }
                      in do result <- mapM mkBillEntry formattedBills
                            simpleSuccess result


composeNothings :: Monad m => m (Maybe x) -> m (Maybe x) -> m (Maybe x)
composeNothings l r =
    l >>= maybe r (return . Just)


composeLNothings :: Monad m => [m (Maybe x)] -> m (Maybe x)
composeLNothings = foldr1 composeNothings

execStatements_ :: SQLiteHandle -> [(String, [(String, Value)])] -> IO (Maybe String)
execStatements_ db xs =
    composeLNothings $ map (uncurry $ execParamStatement_ db) xs

handle_change_bill :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_change_bill db rq =
    let description = getInput rq "description"
        date = validate_date $ getInput rq "date"
        ident :: Int64
        ident = read $ getInput rq "id"
        charges = parseJsonChargeRecords $ forceMaybe $ parseJSON $ getInput rq "charges"
        tot_amount = sum $ map cr_amount charges
        insertCharge charge =
            ("INSERT INTO charges (\"bill\", \"user\", \"amount\") values (:bill, :user, :amount);",
             [(":bill", Int ident),
              (":user", Text $ cr_user charge),
              (":amount", Double $ cr_amount charge)])
    in
      if tot_amount < -0.01 || tot_amount > 0.01
      then simpleError $ "expected total to be zero; was " ++ (show tot_amount)
      else
          do my_uname <- whoAmI rq
             res <- liftIO $ sqlTransaction db $
                    do billOwner <- getBillOwner db ident
                       if billOwner /= my_uname
                        then error "You can't change other people's bills"
                        else
                            execStatements_ db $
                                [("UPDATE bills SET date = :date, description = :description WHERE billident = :ident;",
                                  [(":date", Text date),
                                   (":description", Text description),
                                   (":ident", Int ident)]),
                                 ("DELETE FROM charges WHERE bill = :billident",
                                  [(":billident", Int ident)])] ++
                                map insertCharge charges
             case res of
               Nothing -> trivialSuccess
               Just msg -> simpleError msg

get_bill_date_description :: SQLiteHandle -> Integer -> IO (String, String)
get_bill_date_description db ident =
    do (bills::(Either String [[Row Value]])) <-
           execParamStatement db "SELECT \"date\", \"description\" FROM bills WHERE billident = :ident ORDER BY date DESC;"
                [(":ident", Int $ fromInteger ident)]
       case bills of 
         Left msg -> error msg
         Right [] -> error ("bill " ++ (show ident) ++ " doesn't exist")
         Right [x] ->
             let unwrap :: Maybe Value -> String
                 unwrap (Just (Text s)) = s
                 x' = concat x
                 date = unwrap $ lookup "date" x'
                 description = unwrap $ lookup "description" x'
             in return (date, description)
             

whoAmI :: MonadIO m => Request -> m String
whoAmI rq = cookieUname $ getInput rq "cookie"

getBillOwner :: SQLiteHandle -> Int64 -> IO String
getBillOwner db ident =
    do r0 <- execParamStatement db
             "SELECT owner FROM bills WHERE billident = :ident"
             [(":ident", Int ident)]
       case r0 of
         Left msg -> error $ "Getting bill owner: " ++ msg
         Right x ->
             case forceLookup "owner" $ head $ concat x of
               Text y -> return y

handle_remove_bill :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_remove_bill db rq =
    let ident = read $ getInput rq "id"
    in do my_uname <- whoAmI rq
          res <- liftIO $ sqlTransaction db $
                 do r0 <- getBillOwner db ident
                    if r0 /= my_uname
                     then error "You can't remove bills which are owned by other people"
                     else return ()
                    r1 <- execParamStatement_ db
                          "DELETE FROM charges WHERE bill = :ident"
                          [(":ident", Int ident)]
                    r2 <- execParamStatement_ db
                          "DELETE FROM bills WHERE billident = :ident"
                          [(":ident", Int ident)]
                    return $ maybeErrListToMaybeErr [r1, r2]
          case res of
            Nothing -> trivialSuccess
            Just msg -> simpleError msg
            
handle_clone_bill :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_clone_bill db rq =
    let ident = read $ getInput rq "id"
    in
      do my_uname <- cookieUname $ getInput rq "cookie"
         (date, description) <- liftIO $ get_bill_date_description db ident
         charges' <- liftIO $ execParamStatement db "SELECT \"user\", \"amount\" FROM charges WHERE bill = :ident;"
                     [(":ident", Int $ fromInteger ident)]
         case charges' of
           Left msg -> simpleError msg
           Right charges ->
               let formatCharge charge =
                          (user, amount) where
                              (Text user) = forceLookup "user" charge
                              (Double amount) = forceLookup "amount" charge
                   formattedCharges = map formatCharge $ concat charges
                   insertCharge bill (user, amount) =
                       execParamStatement_ db
                          "INSERT INTO charges (\"bill\", \"user\", \"amount\") VALUES (:bill, :user, :amount);"
                          [(":bill", Int $ fromInteger bill),
                           (":user", Text user),
                           (":amount", Double amount)]
               in do res <- liftIO $ sqlTransaction db $
                            do r1 <- execParamStatement_ db
                                     "INSERT INTO bills (\"date\", \"description\", \"owner\") VALUES (:date, :description, :owner);"
                                     [(":date", Text date),
                                      (":description", Text description),
                                      (":owner", Text my_uname)]
                               case r1 of
                                 Just err -> return $ Just err
                                 Nothing ->
                                     do bill <- getLastRowID db
                                        r2 <- mapM (insertCharge bill) formattedCharges
                                        return $ maybeErrListToMaybeErr r2
                     return $ resultBS 200 $ jsonResponse $
                            case res of
                              Nothing -> JObj [("result", JString "okay")]
                              Just err -> jsonError err

handle_add_user :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_add_user db rq =
    let inputs = rqInputs rq
        saneInputs = map (second $ \x ->
                              if BSL.length (inputValue x) > 100
                              then "_too_long_"
                              else bslToString (inputValue x)) inputs
        uname = map filterCharacterUname $ lookupDefault "username" saneInputs ""
        cookie = getInput rq "cookie"
    in
    do is_admin <- isAdmin cookie
       if not is_admin then simpleError "need to be admin to add users"
        else
        do
          res <- liftIO $ execParamStatement_ db
                 "insert into users (\"username\") values (:user);"
                 [(":user", Text uname)]
          return $ resultBS 200 $ jsonResponse $ JObj $ 
              case res of
                Nothing -> [("result", JString "okay")]
                Just "column username is not unique" ->
                    [("result", JString "error"),
                     ("error", JString $ "Username " ++ uname ++ " already exists")]
                Just msg ->
                    [("result", JString "error"),
                     ("error", JString msg)]


handle_change_passwd :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_change_passwd db rq =
    let uname = getInput rq "username"
        passwd = getInput rq "password"
    in do is_admin <- isAdmin $ getInput rq "cookie"
          my_uname <- cookieUname $ getInput rq "cookie"
          if not is_admin && my_uname /= uname
           then simpleError "Need to be admin to change someone else's password"
           else
           do r <- liftIO $ execParamStatement_ db
                        "UPDATE users SET \"password\" = :password WHERE lower(\"username\") = lower(:uname);"
                        [(":uname", Text uname),
                         (":password", Text passwd)]
              case r of
                Nothing -> trivialSuccess
                Just e -> simpleError e

handle_set_admin :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_set_admin db rq =
    let uname = getInput rq "username"
        want_admin =
            case getInput rq "is_admin" of
              "1" -> "1"
              "0" -> ""
              _ -> error "strange is_admin value"
    in do is_admin <- isAdmin $ getInput rq "cookie"
          my_uname <- cookieUname $ getInput rq "cookie"
          if not is_admin
           then simpleError "Need to be admin to change administrator flag"
           else if map toLower my_uname == map toLower uname
           then simpleError "Can't change your own admin flag"
           else 
            do r <- liftIO $ execParamStatement_ db
                        "UPDATE users SET \"is_admin\" = :is_admin WHERE lower(\"username\") = lower(:uname);"
                        [(":uname", Text uname),
                         (":is_admin", Text want_admin)]
               case r of
                 Nothing -> trivialSuccess
                 Just e -> simpleError e

{- Mapping from cookies to user names -}
login_tokens :: IORef [(String, (String, Bool))]
{-# NOINLINE login_tokens #-}
login_tokens =
    unsafePerformIO $ newIORef []

make_login_token :: String -> Bool -> IO String
make_login_token uname is_admin =
    do (asInt::Integer) <- randomIO
       let res = show $ abs asInt
       modifyIORef login_tokens $ (:) (res,(uname,is_admin))
       return res

isAdmin :: MonadIO m => String -> m Bool
isAdmin cookie =
    do tokns <- liftIO $ readIORef login_tokens
       case lookup cookie tokns of
         Just (_, x) -> return x
         Nothing -> return False

cookieUname :: MonadIO m => String -> m String
cookieUname cookie =
    do tokns <- liftIO $ readIORef login_tokens
       case lookup cookie tokns of
         Just (x, _) -> return x
         Nothing -> error "bad cookie"

handle_login :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_login db rq =
    let uname = getInput rq "uname"
        password = getInput rq "password"
    in do r <- liftIO $ execParamStatement db "SELECT username, is_admin FROM users WHERE lower(username) = lower(:uname) AND (password = :password OR (password ISNULL AND :password = \"\"));"
               [(":uname", Text uname),
                (":password", Text password)]
          case r of
            Left msg -> simpleError msg
            Right (x::[[Row Value]]) ->
                case concat x of
                  [] -> simpleError "login failed"
                  [is_admin'] ->
                      let is_admin =
                            case lookup "is_admin" is_admin' of
                              Just (Text "1") -> True
                              _ -> False
                          is_admin_str = if is_admin then "1"
                                         else ""
                          real_uname =
                            case lookup "username" is_admin' of
                              Just (Text y) -> y
                              _ -> error "Huh?"
                      in
                      do cookie <- liftIO $ make_login_token real_uname is_admin
                         let newUrl = "/index.html?cookie=" ++ cookie ++ "&uname=" ++ real_uname ++ "&is_admin=" ++ is_admin_str
                         return $ addHeader "Location" newUrl $
                                resultBS 303 $ stringToBSL
                                             "<html><head><title>Redirect</title></head><Body>Redirecting...</body></html>"
                  _ -> simpleError "database corrupt"

main :: IO ()
main =
    do db <- openConnection "local/bills.db"
       execStatement_ db "PRAGMA foreign_keys = on;"
       simpleHTTP nullConf
                      [dir "action"
                               [dir "add_user"
                                        [withRequest $ handle_add_user db],
                                dir "get_user_list"
                                    [handle_get_user_list db],
                                dir "remove_user"
                                    [withRequest $ handle_remove_user db],
                                dir "change_password"
                                    [withRequest $ handle_change_passwd db],
                                dir "set_admin"
                                    [withRequest $ handle_set_admin db],
                                dir "add_bill"
                                    [withRequest $ handle_add_bill db],
                                dir "change_bill"
                                    [withRequest $ handle_change_bill db],
                                dir "clone_bill"
                                    [withRequest $ handle_clone_bill db],
                                dir "old_bills"
                                    [handle_old_bills db],
                                dir "remove_bill"
                                    [withRequest $ handle_remove_bill db],
                                dir "login"
                                    [withRequest $ handle_login db],
                                dir "remove_bill_attachment"
                                    [withRequest $ handle_remove_bill_attachment db],
                                dir "fetch_attachment"
                                    [withRequest $ handle_fetch_attachment db]
                               ],
                       dir "forms"
                           [dir "attach_file"
                                    [withRequest $ handle_attach_file db]],
                       fileServe ["index.html", "jquery.js",
                                  "data.js", "jquery.bgiframe.js",
                                  "jquery.datePicker.js", "user_admin.js",
                                  "util.js", "old_bills.js", "add_bill.js",
                                  "balances.js", "login.html",
                                  "attach_file.html"]
                                      "static"
                      ]
