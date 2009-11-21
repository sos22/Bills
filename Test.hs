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
forceMaybe = maybe (error "whoops") id

forceLookup :: Eq a => a -> [(a, b)] -> b
forceLookup key table = forceMaybe $ lookup key table

lookupDefault :: Eq a => a -> [(a,b)] -> b -> b
lookupDefault key table def =
    maybe def id (lookup key table)

filterCharacterUname :: Char -> Char
filterCharacterUname x | (isAlpha x || isDigit x || x == '_') = x
                       | otherwise = '_'

filterCharacterFullName :: Char -> Char
filterCharacterFullName x | (isAlpha x || isDigit x || x == '_' || x == ' ')
                              = x
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

handle_get_user_list :: (MonadIO m) => SQLiteHandle -> m Response
handle_get_user_list db =
    let doOneEntry :: Row String -> JSON
        doOneEntry r =
            let uname = forceLookup "username" r
                fullname = forceLookup "fullname" r
            in JObj [("uname", JString uname),
                     ("fullname", JString fullname)]
    in
    do res <- liftIO $ execStatement db
              "select \"username\", \"fullname\" from users;"
       return $ resultBS 200 $ jsonResponse $
        case res of
         Left msg -> jsonError msg
         Right r -> JObj [("result", JString "okay"),
                          ("data", JList $ map doOneEntry $ concat r)]

getInput :: Request -> String -> String
getInput rq key =
    let res = inputValue $ forceLookup key $ rqInputs rq
    in if BSL.length res > 100 then "_too_long_"
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
        deMaybe [] = []
        deMaybe (Just x : xs) = x:(deMaybe xs)
        deMaybe (Nothing:xs) = deMaybe xs

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
                                 let errs = deMaybe r2 ++ deMaybe r3
                                 case errs of
                                   [] -> return Nothing
                                   _ -> return $ Just $ foldr (\a b -> a ++ ", " ++ b) "" errs
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
                                               ("fullname", JString user),
                                               ("charge", (JFloat $ realToFrac charge))]
                                               |
                                               (ident, user, charge) <- be_charges be])]

handle_old_bills :: (MonadIO m) => SQLiteHandle -> m Response
handle_old_bills db =
    do bills' <- liftIO $ execStatement db "select * from bills;"
       case bills' of
         Left msg -> simpleError msg
         Right bills ->
             do charges' <- liftIO $ execStatement db "select * from charges;"
                case charges' of
                  Left msg -> simpleError msg
                  Right charges ->
                      let lookupUser uname =
                             do r <- liftIO $ execParamStatement db
                                     "select fullname from users where username = :uname;"
                                     [((":uname"), (Text uname))]
                                case r of
                                  Right ([r']:_) ->
                                      return $ snd $ head $ r'
                                  _ -> return $ "unknown user " ++ uname

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
                      in do translatedCharges <- mapM (\(i, a, b, c) ->
                                                           do b' <- lookupUser b
                                                              return (i, a, b', c))
                                                 formattedCharges
                            let chargesForBill bill =
                                    [(ident, user, realToFrac amount) |
                                     (ident, bill', user, amount) <- translatedCharges, bill' == bill]
                                result =
                                    map (\(ident, date, description) ->
                                             BillEntry (fromInteger $ toInteger ident) date description (chargesForBill ident)) formattedBills
                            simpleSuccess result


handle_add_user :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_add_user db rq =
    let inputs = rqInputs rq
        saneInputs = map (second $ \x ->
                              if BSL.length (inputValue x) > 100
                              then "_too_long_"
                              else bslToString (inputValue x)) inputs
        uname = map filterCharacterUname $ lookupDefault "username" saneInputs ""
        fullname = map filterCharacterFullName $ lookupDefault "full_name" saneInputs ""
    in
    do res <- liftIO $ execParamStatement_ db
              "insert into users (\"username\", \"fullname\") values (:user, :full);"
              [(":user", Text uname), (":full", Text fullname)]
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
                                dir "old_bills"
                                    [handle_old_bills db]
                               ]
                      , fileServe ["index.html", "jquery.js",
                                   "data.js", "jquery.bgiframe.js",
                                   "jquery.datePicker.js"] "static"
                      ]
