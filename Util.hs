{- Various bits and bobs which aren't really specific to what we're
   trying to do here. -}
module Util({- BSL manipulations -}
            stringToBSL, bslToString, safeBslToString,
            bslToBS,

            {- Various datastructure manipulations -}
            lookupDefault, forceLookup, forceMaybe,
            deEither, deMaybe, rightMap,
            maybeErrListToMaybeErr,

            {- Other stuff -}
            second, cond, composeLNothings,
            validate_date
           ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Char

second :: (a -> b) -> (c, a) -> (c, b)
second f (a, b) = (a, f b)

stringToBSL :: String -> BSL.ByteString
stringToBSL = BSL.pack . (map (fromInteger.toInteger.ord))

bslToString :: BSL.ByteString -> String
bslToString = map (chr . fromInteger . toInteger) . BSL.unpack

{- Like bslToString, but only for suitably small BSLs -}
safeBslToString :: BSL.ByteString -> String
safeBslToString bsl =
    if BSL.length bsl > 10000
    then error "unpack very large BSL"
    else bslToString bsl

bslToBS :: BSL.ByteString -> BS.ByteString
bslToBS = BS.concat . BSL.toChunks

forceMaybe :: Maybe a -> a
forceMaybe = maybe (error "whoops forced Nothing") id

forceLookup :: Eq a => a -> [(a, b)] -> b
forceLookup key table = forceMaybe $ lookup key table

lookupDefault :: Eq a => a -> [(a,b)] -> b -> b
lookupDefault key table def =
    maybe def id (lookup key table)

rightMap :: (a -> b) -> Either c a -> Either c b
rightMap f (Right x) = Right $ f x
rightMap _ (Left x) = Left x

deEither :: [Either a b] -> Either a [b]
deEither [] = Right []
deEither ((Left msg):_) = Left msg
deEither ((Right x):xs) =
    case deEither xs of
      Left msg -> Left msg
      Right xs' -> Right (x:xs')

deMaybe :: [Maybe x] -> [x]
deMaybe [] = []
deMaybe (Just x : xs) = x:(deMaybe xs)
deMaybe (Nothing:xs) = deMaybe xs

cond :: a -> a -> Bool -> a
cond t _ True = t
cond _ f False = f

composeNothings :: Monad m => m (Maybe x) -> m (Maybe x) -> m (Maybe x)
composeNothings l r =
    l >>= maybe r (return . Just)

{- Compose monads returning Maybes, so that if any return a Just x we
   return the first such Just x, and otherwise we return Nothing.
   This is pretty much just Maybe >>= lifted into another monad.  -}
composeLNothings :: Monad m => [m (Maybe x)] -> m (Maybe x)
composeLNothings = foldr1 composeNothings

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
        split_on key the_string =
            foldr (\c accumulated ->
                       case accumulated of
                         [] -> if c == key
                               then [[]]
                               else [[c]]
                         (acc1:accs) ->
                             if c == key
                             then []:accumulated
                             else (c:acc1):accs) [] the_string
        year :: Int
        year = read year'
        month :: Int
        month = read month'
        day :: Int
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
                                       else 28
                       _ -> error "bizarre month")
           then error $ "Bad day of month " ++ what
           else what

