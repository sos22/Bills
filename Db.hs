{- Low-level database helpers.  A pretty thin wrapper around the
   sqlite bindings. -}
module Db(dbStatement, dbParamStatement, sqlTransaction,
          execStatements_,
          rowDouble, rowString) where

import Database.SQLite
import Control.Exception
import Debug.Trace

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
      _ -> error $ "Type error getting double " ++ key

rowString :: String -> Row Value -> String
rowString key row =
    case forceLookup key row of
      Text x -> x
      _ -> error $ "Type error getting string " ++ key

sqlTransaction :: SQLiteHandle -> IO x -> IO x
sqlTransaction db body =
    do r <- execStatement_ db "BEGIN TRANSACTION;"
       case r of
         Just msg -> error ("failed to begin transaction: " ++ msg)
         Nothing ->
             do res <- Control.Exception.onException body (execStatement_ db "ROLLBACK;")
                r2 <- execStatement_ db "COMMIT;"
                case r2 of
                  Just x -> trace ("failed to commit: " ++ x) $ sqlTransaction db body
                  Nothing -> return res
