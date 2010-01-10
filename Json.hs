module Json(jsonResponse, jsonError, parseJSON,
            FromJSON(..)) where

import qualified Data.ByteString.Lazy as BSL

import HAppS.Server.JSON
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim hiding (getInput)
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Char
import GHC.Int

import Util
import Debug.Trace

instance ToJSON a => ToJSON [a] where
    toJSON xs = JList $ map toJSON xs

instance ToJSON Int64 where
    toJSON i = JInt $ fromInteger $ toInteger i

instance ToJSON Int where
    toJSON i = JInt i

instance ToJSON Integer where
    toJSON i = JInt $ fromInteger i

instance ToJSON Double where
    toJSON d = JFloat $ realToFrac d

class FromJSON a where
    fromJSON :: JSON -> Maybe a

instance FromJSON a => FromJSON [a] where
    fromJSON (JList i) = nothingIfAnythingNothing $ map fromJSON i
                         where
                           nothingIfAnythingNothing [] = Just []
                           nothingIfAnythingNothing (Nothing:_) = Nothing
                           nothingIfAnythingNothing ((Just x):o) =
                               case nothingIfAnythingNothing o of
                                 Nothing -> Nothing
                                 Just o' -> Just (x:o')
    fromJSON _ = Nothing

jsonResponse :: JSON -> BSL.ByteString
jsonResponse = stringToBSL. jsonToString

jsonError :: String -> JSON
jsonError msg = JObj [("result", JString "error"),
                      ("error", JString msg)]

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

