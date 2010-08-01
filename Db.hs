{- Low-level database helpers.  A pretty thin wrapper around the
   sqlite bindings. -}
module Db(dbStatement, dbParamStatement, sqlTransaction,
          execStatements_,
          rowDouble, rowString, rowInt, rowBlob) where

import qualified Data.ByteString as BS
import Database.SQLite
import Control.Exception
import Debug.Trace
import GHC.Int

import Util

execStatements_ :: SQLiteHandle -> [(String, [(String, Value)])] -> IO (Maybe String)
execStatements_ db xs =
    composeLNothings $ map (uncurry $ execParamStatement_ db) xs

dbParamStatement :: SQLiteHandle -> String -> [(String, Value)] -> IO (Either String [Row Value])
dbParamStatement db query params =
    do res <- execParamStatement db query params
       return $ rightMap concat res

dbStatement :: SQLiteHandle -> String -> IO (Either String [Row Value])
dbStatement db query =
    do res <- execStatement db query
       return $ rightMap concat res

rowDouble :: String -> Row Value -> Double
rowDouble key row =
    case forceLookup key row of
      Double x -> x
      Null -> 0
      _ -> error $ "Type error getting double " ++ key

rowString :: String -> Row Value -> String
rowString key row =
    case forceLookup key row of
      Text x -> x
      _ -> error $ "Type error getting string " ++ key

rowInt :: String -> Row Value -> Int64
rowInt key row =
    case forceLookup key row of
      Int x -> x
      _ -> error $ "Type error getting int " ++ key

rowBlob :: String -> Row Value -> BS.ByteString
rowBlob key row =
    case forceLookup key row of
      Blob x -> x
      _ -> error $ "Type error getting blob " ++ key

onException :: IO a -> IO b -> IO a
onException io what = io `Control.Exception.catch`
                      (\e -> do _ <- what
                                throw (e::SomeException))

sqlTransaction :: SQLiteHandle -> IO x -> IO x
sqlTransaction db body =
    do r <- execStatement_ db "BEGIN TRANSACTION;"
       case r of
         Just msg -> error ("failed to begin transaction: " ++ msg)
         Nothing ->
             do res <- Db.onException body (execStatement_ db "ROLLBACK;")
                r2 <- execStatement_ db "COMMIT;"
                case r2 of
                  Just x -> trace ("failed to commit: " ++ x) $ sqlTransaction db body
                  Nothing -> return res

