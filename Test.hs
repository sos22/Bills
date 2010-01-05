{-# LANGUAGE StandaloneDeriving #-}
module Main(main) where

import qualified Data.ByteString.Lazy as BSL

import HAppS.Server
import HAppS.Server.JSON
import Control.Monad.Trans
import Control.Exception
import Char
import Database.SQLite
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
    in do res <- liftIO $ execParamStatement_ db
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

handle_add_bill :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_add_bill db rq =
    let description = getInput rq "description"
        date = getInput rq "date"
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
           do res <- liftIO $ sqlTransaction db $
                     do r1 <- execParamStatement_ db
                              "INSERT into bills (\"date\", \"description\") values (:date, :description);"
                              [(":date", Text date), (":description", Text description)]
                        case r1 of
                          Just err -> return $ Just err
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
                             be_charges :: [(Int, String, Double)] } deriving Show

instance ToJSON BillEntry where
    toJSON be = JObj [("ident", JInt $ be_ident be),
                      ("date", JString $ be_date be),
                      ("description", JString $be_description be),
                      ("charges", JList [JObj [("ident", JInt ident),
                                               ("charge", (JFloat $ realToFrac charge)),
                                               ("uname", (JString user))]
                                               |
                                               (ident, user, charge) <- be_charges be])]

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
                              (ident, date, description) where
                                  (Int ident) = forceLookup "billident" bill
                                  (Text date) = forceLookup "date" bill
                                  (Text description) = forceLookup "description" bill
                          formattedCharges = map formatCharge $ concat charges
                          formattedBills = map formatBill $ concat bills
                      in do 
                            let chargesForBill bill =
                                    [(ident, user, realToFrac amount) |
                                     (ident, bill', user, amount) <- formattedCharges, bill' == bill]
                                result =
                                    map (\(ident, date, description) ->
                                             BillEntry (fromInteger $ toInteger ident) date description (chargesForBill ident)) formattedBills
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
        date = getInput rq "date"
        ident :: Integer
        ident = read $ getInput rq "id"
        charges = parseJsonChargeRecords $ forceMaybe $ parseJSON $ getInput rq "charges"
        tot_amount = sum $ map cr_amount charges
        insertCharge charge =
            ("INSERT INTO charges (\"bill\", \"user\", \"amount\") values (:bill, :user, :amount);",
             [(":bill", Int $ fromInteger ident),
              (":user", Text $ cr_user charge),
              (":amount", Double $ cr_amount charge)])
    in
      if tot_amount < -0.01 || tot_amount > 0.01
      then simpleError $ "expected total to be zero; was " ++ (show tot_amount)
      else
          do res <- liftIO $ sqlTransaction db $
                    execStatements_ db $
                      [("UPDATE bills SET date = :date, description = :description WHERE billident = :ident;",
                        [(":date", Text date),
                         (":description", Text description),
                         (":ident", Int $ fromInteger ident)]),
                       ("DELETE FROM charges WHERE bill = :billident",
                        [(":billident", Int $ fromInteger ident)])] ++
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
             

handle_remove_bill :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_remove_bill db rq =
    let ident = read $ getInput rq "id"
    in do res <- liftIO $ sqlTransaction db $
                 do r1 <- execParamStatement_ db
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
      do (date, description) <- liftIO $ get_bill_date_description db ident
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
                                     "INSERT INTO bills (\"date\", \"description\") VALUES (:date, :description);"
                                     [(":date", Text date),
                                      (":description", Text description)]
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
    in
    do res <- liftIO $ execParamStatement_ db
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
                                dir "add_bill"
                                    [withRequest $ handle_add_bill db],
                                dir "change_bill"
                                    [withRequest $ handle_change_bill db],
                                dir "clone_bill"
                                    [withRequest $ handle_clone_bill db],
                                dir "old_bills"
                                    [handle_old_bills db],
                                dir "remove_bill"
                                    [withRequest $ handle_remove_bill db]
                               ]
                      , fileServe ["index.html", "jquery.js",
                                   "data.js", "jquery.bgiframe.js",
                                   "jquery.datePicker.js", "user_admin.js",
                                   "util.js", "old_bills.js", "add_bill.js",
                                   "balances.js"]
                                      "static"
                      ]
