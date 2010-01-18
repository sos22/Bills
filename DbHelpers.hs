{-# LANGUAGE ScopedTypeVariables, PatternSignatures #-}
{- Slightly higher-level database operations -}
module DbHelpers(
                 make_login_token, whoAmI, amIAdmin,

                 getBillOwner, get_bill_date_description,
                 attachmentsForBill, findBalance,

                 requireAdmin
                ) where

import HAppS.Server
import Control.Monad.Trans
import Control.Monad.Identity
import Data.IORef
import Random
import System.IO.Unsafe
import GHC.Int
import Database.SQLite

import Db
import Util
import ReqResp
import Types

findBalance :: SQLiteHandle -> String -> IO (Either String Double)
findBalance db uname =
    do charges <- dbParamStatement db
                  "SELECT SUM(amount) FROM charges WHERE user = :user"
                  [(":user", Text uname)]
       return $ rightMap ((rowDouble "SUM(amount)") . concat) charges


attachmentsForBill :: Int64 -> SQLiteHandle -> IO (Either String [Attachment])
attachmentsForBill bill db =
    let formatAttachment att =
            Attachment { at_ident = ident,
                         at_filename = fname}  where
                ident = rowInt "attach_ident" att
                fname = rowString "filename" att
        doFormat = rightMap $ map formatAttachment
    in
    liftM doFormat $
          dbParamStatement db "SELECT attach_ident, filename FROM bill_attachments WHERE bill_ident = :bill"
                               [(":bill", Int bill)]

get_bill_date_description :: SQLiteHandle -> Integer -> IO (String, String)
get_bill_date_description db ident =
    do bills <- dbParamStatement db
                "SELECT \"date\", \"description\" FROM bills WHERE billident = :ident;"
                [(":ident", Int $ fromInteger ident)]
       case bills of 
         Left msg -> error msg
         Right [] -> error ("bill " ++ (show ident) ++ " doesn't exist")
         Right [x] ->
             let date = rowString "date" x
                 description = rowString "description" x
             in return (date, description)
         _ -> error "multiple bills with the same ident?"

getBillOwner :: SQLiteHandle -> Int64 -> IO String
getBillOwner db ident =
    liftM (either (error . ((++) "Getting bill owner: "))
                      ((rowString "owner") . head)) $
    dbParamStatement db
          "SELECT owner FROM bills WHERE billident = :ident"
          [(":ident", Int ident)]


{- The session list should really be in the database, rather than in
   memory, but that's moderately tricky (because you need to be much
   more careful about garbage collection), so cheat a little and keep
   it in an IORef. -}
requireAdmin :: MonadIO m => Request -> m Response -> m Response
requireAdmin rq doit =
    (amIAdmin rq) >>=
    (cond doit (simpleError "Need to be administrator to do that"))

whoAmI :: MonadIO m => Request -> m String
whoAmI rq = cookieUname $ getInput rq "cookie"

{- Mapping from cookies to user names and isAdmin flag -}
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
    liftIO $ liftM ((maybe False snd) . lookup cookie) $
             readIORef login_tokens

amIAdmin :: MonadIO m => Request -> m Bool
amIAdmin rq = isAdmin $ getInput rq "cookie"

cookieUname :: MonadIO m => String -> m String
cookieUname cookie =
    liftIO $ liftM ((maybe (error "bad cookie") fst) . (lookup cookie)) $
             readIORef login_tokens
