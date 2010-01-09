{- Some utilities to make HAppS requests and responses easier to deal
   with -}
module ReqResp where

import HAppS.Server
import HAppS.Server.JSON
import Control.Monad.Trans

import Util
import Json

simpleError :: (Monad m) => String -> m Response
simpleError msg = return $ resultBS 200 $ jsonResponse $ jsonError msg

simpleSuccess :: (Monad m, ToJSON a) => a -> m Response
simpleSuccess what = return $ resultBS 200 $ jsonResponse $
                     JObj [("result", JString "okay"),
                           ("data", toJSON what)]

trivialSuccess :: Monad m => m Response
trivialSuccess = return $ resultBS 200 $ jsonResponse $
                 JObj [("result", JString "okay")]

maybeToResponse :: MonadIO m => Maybe String -> m Response
maybeToResponse = maybe trivialSuccess simpleError

eitherToResponse :: (Monad m, ToJSON b) => Either String b -> m Response
eitherToResponse = either simpleError simpleSuccess

getInput :: Request -> String -> String
getInput rq key =
    safeBslToString $ inputValue $ forceLookup key $ rqInputs rq

