{-# LANGUAGE StandaloneDeriving #-}
module Main(main) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import HAppS.Server
import HAppS.Server.JSON
import Control.Monad.Trans
import Control.Monad.Identity
import Char
import Database.SQLite
import GHC.Int

import Util
import Json
import Db
import ReqResp
import DbHelpers
import Types

filterCharacterUname :: Char -> Char
filterCharacterUname x | (isAlpha x || isDigit x || x == '_') = x
                       | otherwise = '_'

handle_get_user_list :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_get_user_list db _ =
    let doOneEntry :: Row Value -> IO (Either String JSON)
        doOneEntry r =
            let uname = rowString "username" r
                build_return balance =
                    JObj [("uname", JString uname),
                          ("balance", JFloat $ realToFrac balance)]
            in liftM (rightMap build_return) $ findBalance db uname
    in
      liftIO $ do res <- dbStatement db "SELECT \"username\" FROM users;"
                  either simpleError
                             (\r ->
                              (liftM deEither $ mapM doOneEntry r) >>=
                              eitherToResponse) res

handle_remove_user :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_remove_user db rq =
    requireAdmin rq $ (liftIO $ execParamStatement_ db
                       "delete from users where username=:user;"
                       [(":user", Text $ getInput rq "username")]) >>=
                      maybeToResponse

handle_add_bill :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_add_bill db rq =
    let description = getInput rq "description"
        date = validate_date $ getInput rq "date"
        to_pay =
            forceMaybe $ (parseJSON $ getInput rq "to_pay") >>= fromJSON
        to_receive =
            forceMaybe $ (parseJSON $ getInput rq "to_receive") >>= fromJSON
        tot_pay = sum $ map cr_amount to_pay
        tot_recv = sum $ map cr_amount to_receive
        insertPay :: Integer -> ChargeRecord -> IO (Maybe String)
        insertPay bill cr =
            execParamStatement_ db
                 "INSERT INTO charges (\"bill\", \"user\", \"amount\") VALUES (:bill, :user, :amount);"
                 [(":bill", Int $ fromInteger bill),
                  (":user", Text $ cr_user cr),
                  (":amount", Double $ cr_amount cr)]
        insertReceive :: Integer -> ChargeRecord -> IO (Maybe String)
        insertReceive bill cr =
            execParamStatement_ db
                 "INSERT INTO charges (\"bill\", \"user\", \"amount\") VALUES (:bill, :user, :amount);"
                 [(":bill", Int $ fromInteger bill),
                  (":user", Text $ cr_user cr),
                  (":amount", Double (- (cr_amount cr)))]

    in if abs (tot_pay - tot_recv) > 0.01
       then simpleError $ "Total payment " ++ (show tot_pay) ++ " doesn't match total receipt " ++ (show tot_recv)
       else
           do my_uname <- whoAmI rq
              res <- liftIO $ sqlTransaction db $
                     do r1 <- execParamStatement_ db
                              "INSERT into bills (\"date\", \"description\", \"owner\") values (:date, :description, :owner);"
                              [(":date", Text date),
                               (":description", Text description),
                               (":owner", Text my_uname)]
                        case r1 of
                          Just err -> return $ Left ("creating bill owned by " ++ my_uname ++ ": " ++ err)
                          Nothing ->
                              do bill <- getLastRowID db
                                 r2 <- mapM (insertPay bill) to_pay
                                 r3 <- mapM (insertReceive bill) to_receive
                                 case maybeErrListToMaybeErr (r2 ++ r3) of
                                   Just err -> return $ Left err
                                   Nothing -> return $ Right bill
              eitherToResponse res

maximumUploadSize :: Int64
maximumUploadSize = 500000

handle_remove_bill_attachment :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_remove_bill_attachment db rq =
    let attachIdent = read $ getInput rq "id"
    in
    do my_uname <- whoAmI rq
       r <- liftIO $ dbParamStatement db
            "SELECT bills.owner FROM bills,bill_attachments WHERE bill_attachments.attach_ident = :ident AND bills.billident = bill_attachments.bill_ident;"
            [(":ident", Int attachIdent)]
       case r of
         Left msg -> simpleError msg
         Right x ->
             if (rowString "owner" $ head x) /= my_uname
             then simpleError "can't remove attachments you don't own"
             else
                 liftIO $ execParamStatement_ db
                            "DELETE FROM bill_attachments WHERE attach_ident = :ident;"
                            [(":ident", Int attachIdent)] >>=
                            maybeToResponse
               
handle_fetch_statement :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_fetch_statement db rq =
    let username = getInput rq "username"
        start_date = getInput rq "start-date"
        end_date = getInput rq "end-date"
        add_attachments :: Int64 -> StatementEntry -> IO (Either String StatementEntry)
        add_attachments bill stmt =
            liftM (rightMap $ \attaches' ->
                       (stmt { se_attachments = attaches' })) $
                      attachmentsForBill bill db
        start_balance =
            do r <- dbParamStatement db
                    ("SELECT SUM(charges.amount) FROM bills, charges " ++
                     "WHERE bills.date < :start_date " ++
                     "AND bills.billident == charges.bill " ++
                     "AND charges.user == :uname")
                    [(":start_date", Text start_date),
                     (":uname", Text username)]
               return $ case r of
                          Left msg -> error msg
                          Right x ->
                              case forceLookup "SUM(charges.amount)" $ head x of
                                Null -> 0
                                Double y -> y
                                _ -> error "typing screw up getting starting balance"
    in do r <- liftIO $ dbParamStatement db 
               ("SELECT bills.billident, bills.date, bills.description, charges.amount FROM bills, charges " ++
                "WHERE bills.date >= :start_date " ++
                "AND bills.date <= :end_date " ++
                "AND bills.billident == charges.bill " ++
                "AND charges.user == :uname " ++
                "ORDER BY bills.date")
               [(":start_date", Text start_date),
                (":end_date", Text end_date),
                (":uname", Text username)]
          starting_balance <- liftIO $ start_balance
          case r of
            Left msg -> simpleError msg
            Right rows ->
                let {- Extract the bill ident, date, description, and quantity
                       from a query row. -}
                    {- The result doesn't have attachments or balance_after -}
                    parse_row :: Row Value -> (Int64, StatementEntry)
                    parse_row row =
                        (rowInt "billident" row,
                         StatementEntry { se_date = rowString "date" row,
                                          se_description = rowString "description" row,
                                          se_amount = rowDouble "amount" row,
                                          se_balance_after = undefined,
                                          se_attachments = undefined} )
                    parsed_rows = map parse_row rows

                    {- Set the post balances on the statement entries -}
                    add_post_balances :: Double -> [StatementEntry] -> [StatementEntry]
                    add_post_balances _ [] = []
                    add_post_balances start (stmt:others) =
                        (stmt { se_balance_after = start + se_amount stmt}):
                        (add_post_balances (start + se_amount stmt) others)

                    mkStatement :: [StatementEntry] -> Statement
                    mkStatement = (Statement starting_balance) . (add_post_balances starting_balance)
                in liftIO $
                      (eitherToResponse .
                       rightMap (toJSON . mkStatement) .
                       deEither) =<<
                      mapM (uncurry add_attachments) parsed_rows

handle_fetch_attachment :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_fetch_attachment db rq =
    let ident = read $ getInput rq "id"
    in do r <- liftIO $ dbParamStatement db
               "SELECT content FROM bill_attachments WHERE attach_ident = :ident;"
               [(":ident", Int ident)]
          case r of
            Left msg -> simpleError msg
            Right x ->
                let content = rowBlob "content" $ head x
                in return $ setHeader "content-type" "application/octet-stream" $ resultBS 200 $ BSL.fromChunks [content]

handle_attach_file :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_attach_file db rq =
    let body = bodyInput rq
        file = forceLookup "file_to_attach" body
        file_body = inputValue file
        file_name = forceMaybe $ inputFilename file
        billIdent :: Int64
        billIdent = read $ safeBslToString $ inputValue $ forceLookup "bill" body
        referer = map (chr . fromInteger . toInteger) $ BS.unpack $ forceMaybe $ getHeader "Referer" rq
    in
      if BSL.length file_body > maximumUploadSize
      then simpleError "File is too big to attach"
      else
          do my_uname <- whoAmI rq
             billOwner <- liftIO $ getBillOwner db billIdent
             if billOwner /= my_uname
              then simpleError "Can't attach files to someone else's bills"
              else
                  do r <- liftIO $ execParamStatement_ db
                          "INSERT INTO bill_attachments (\"bill_ident\", \"content\", \"filename\") VALUES (:bill_ident, :content, :filename);"
                          [(":bill_ident", Int billIdent),
                           (":content", Blob $ bslToBS file_body),
                           (":filename", Text file_name)]
                     case r of
                       Just msg -> simpleError msg
                       Nothing ->
                           do attachIdent <- liftIO $ getLastRowID db
                              return $ addHeader "Location" referer $
                                     addHeader "X-AttachmentId" (show attachIdent) $
                                     resultBS 303 $ stringToBSL $
                                              "<html><body><a href=\"" ++ referer ++ "\">Redirecting...</a></body></html>"

handle_old_bills :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_old_bills db _ =
    do bills' <- liftIO $ dbStatement db "SELECT * FROM bills ORDER BY date DESC;"
       case bills' of
         Left msg -> simpleError msg
         Right bills ->
             do charges' <- liftIO $ dbStatement db "SELECT * FROM CHARGES;"
                case charges' of
                  Left msg -> simpleError msg
                  Right charges ->
                      let 
                          formatCharge charge =
                              (fromInteger $ toInteger ident, bill, user, amount) where
                                  ident = rowInt "chargeident" charge
                                  bill = rowInt "bill" charge
                                  user = rowString "user" charge
                                  amount = rowDouble "amount" charge
                          formatBill bill =
                              (ident, date, description, owner) where
                                  ident = rowInt "billident" bill
                                  date = rowString "date" bill
                                  description = rowString "description" bill
                                  owner =
                                      case forceLookup "owner" bill of
                                        Text o -> o
                                        Null -> ""
                                        _ -> error "typing screw up getting owner of bill"
                          formattedCharges = map formatCharge charges
                          formattedBills = map formatBill bills
                          chargesForBill bill =
                               [BillCharge {bc_ident = ident,
                                            bc_who = user,
                                            bc_amount = amount} |
                                (ident, bill', user, amount) <- formattedCharges, bill' == bill]
                          mkBillEntry (ident, date, description, owner) =
                               let mk_be attachments =
                                       BillEntry { be_ident = fromInteger $ toInteger ident,
                                                   be_date = date,
                                                   be_description = description,
                                                   be_owner = owner,
                                                   be_charges = chargesForBill ident,
                                                   be_attachments = attachments }
                               in liftM (rightMap mk_be) $ attachmentsForBill ident db
                      in liftIO $ (liftM deEither $ mapM mkBillEntry formattedBills) >>= 
                                  eitherToResponse

handle_change_bill :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_change_bill db rq =
    let description = getInput rq "description"
        date = validate_date $ getInput rq "date"
        ident :: Int64
        ident = read $ getInput rq "id"
        charges = forceMaybe $ (parseJSON $ getInput rq "charges") >>= fromJSON
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
             maybeToResponse res

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
          maybeToResponse res
            
handle_clone_bill :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_clone_bill db rq =
    let ident = read $ getInput rq "id"
    in
      do my_uname <- whoAmI rq
         (date, description) <- liftIO $ get_bill_date_description db ident
         charges' <- liftIO $ dbParamStatement db "SELECT \"user\", \"amount\" FROM charges WHERE bill = :ident;"
                     [(":ident", Int $ fromInteger ident)]
         case charges' of
           Left msg -> simpleError msg
           Right charges ->
               let formatCharge charge =
                          (user, amount) where
                              (Text user) = forceLookup "user" charge
                              (Double amount) = forceLookup "amount" charge
                   formattedCharges = map formatCharge charges
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
                                 Just err -> return $ Left err
                                 Nothing ->
                                     do bill <- getLastRowID db
                                        r2 <- mapM (insertCharge bill) formattedCharges
                                        case maybeErrListToMaybeErr r2 of
                                          Nothing -> return $ Right bill
                                          Just err -> return $ Left err
                     eitherToResponse res

handle_add_user :: (MonadIO m) => SQLiteHandle -> Request -> m Response
handle_add_user db rq =
    let uname = map filterCharacterUname $ getInput rq "username"
    in
      requireAdmin rq $
      do res <- liftIO $ execParamStatement_ db
                "insert into users (\"username\") values (:user);"
                [(":user", Text uname)]
         case res of
           Nothing -> trivialSuccess
           Just "column username is not unique" ->
               simpleError $ "Username " ++ uname ++ " already exists"
           Just msg -> simpleError msg

handle_change_passwd :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_change_passwd db rq =
    let uname = getInput rq "username"
        passwd = getInput rq "password"
    in do is_admin <- amIAdmin rq
          my_uname <- whoAmI rq
          if not is_admin && my_uname /= uname
           then simpleError "Need to be admin to change someone else's password"
           else
           liftIO $ execParamStatement_ db
                      "UPDATE users SET \"password\" = :password WHERE lower(\"username\") = lower(:uname);"
                      [(":uname", Text uname),
                       (":password", Text passwd)] >>=
                      maybeToResponse

handle_set_admin :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_set_admin db rq =
    let uname = getInput rq "username"
        want_admin =
            case getInput rq "is_admin" of
              "1" -> "1"
              "0" -> ""
              _ -> error "strange is_admin value"
    in requireAdmin rq $
       do my_uname <- whoAmI rq
          if map toLower my_uname == map toLower uname
           then simpleError "Can't change your own admin flag"
           else 
            liftIO $ execParamStatement_ db
                       "UPDATE users SET \"is_admin\" = :is_admin WHERE lower(\"username\") = lower(:uname);"
                       [(":uname", Text uname),
                        (":is_admin", Text want_admin)] >>=
                       maybeToResponse

{- Gives you an is_admin, real_username, cookie triple, or an error message. -}
do_login :: MonadIO m => SQLiteHandle -> Request -> m (Either String (Bool, String, String))
do_login db rq = 
    let uname = getInput rq "uname"
        password = getInput rq "password"
    in do r <- liftIO $ dbParamStatement db "SELECT username, is_admin FROM users WHERE lower(username) = lower(:uname) AND (password = :password OR (password ISNULL AND :password = \"\"));"
               [(":uname", Text uname),
                (":password", Text password)]
          case r of
            Left msg -> return $ Left msg
            Right [] -> return $ Left "login failed"
            Right [res] ->
                let is_admin = case lookup "is_admin" res of
                                 Just (Text "1") -> True
                                 _ -> False
                    real_uname = rowString "username" res
                in do cookie <- liftIO $ make_login_token real_uname is_admin
                      return $ Right (is_admin, real_uname, cookie)
            _ -> return $ Left "database corrupt"

handle_login :: MonadIO m => SQLiteHandle -> Request -> m Response
handle_login db rq =
    do r <- do_login db rq
       case r of
         Left msg -> simpleError msg
         Right (is_admin, real_uname, cookie) ->
             let is_admin_str = if is_admin then "1"
                                else ""
                 newUrl = "/index.html?cookie=" ++ cookie ++ "&uname=" ++ real_uname ++ "&is_admin=" ++ is_admin_str
             in return $ addHeader "Location" newUrl $
                         resultBS 303 $ stringToBSL
                                      "<html><head><title>Redirect</title></head><Body>Redirecting...</body></html>"

main :: IO ()
main =
    do db <- openConnection "local/bills.db"
       execStatement_ db "PRAGMA foreign_keys = on;"
       simpleHTTP nullConf
                      [dir "action"
                               [dir "add_user"
                                        [withRequest $ handle_add_user db],
                                dir "get_user_list"
                                    [withRequest $ handle_get_user_list db],
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
                                    [withRequest $ handle_old_bills db],
                                dir "remove_bill"
                                    [withRequest $ handle_remove_bill db],
                                dir "login"
                                    [withRequest $ handle_login db],
                                dir "remove_bill_attachment"
                                    [withRequest $ handle_remove_bill_attachment db],
                                dir "fetch_attachment"
                                    [withRequest $ handle_fetch_attachment db],
                                dir "fetch_statement"
                                    [withRequest $ handle_fetch_statement db]
                               ],
                       dir "forms"
                           [dir "attach_file"
                                    [withRequest $ handle_attach_file db]],
                       fileServe ["index.html", "jquery.js",
                                  "data.js", "jquery.bgiframe.js",
                                  "jquery.datePicker.js", "user_admin.js",
                                  "util.js", "old_bills.js", "add_bill.js",
                                  "balances.js", "login.html",
                                  "attach_file.html", "statements.html",
                                  "display_statement.html" ]
                                      "static"
                      ]
