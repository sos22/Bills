{- Various bits and bobs which aren't really specific to what we're
   trying to do here. -}
module Util({- BSL manipulations -}
            stringToBSL, bslToString, safeBslToString,

            {- Various datastructure manipulations -}
            lookupDefault, forceLookup, forceMaybe,
            deEither, deMaybe, rightMap,

            {- Other stuff -}
            second, cond, composeLNothings
           ) where

import qualified Data.ByteString.Lazy as BSL
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


composeLNothings :: Monad m => [m (Maybe x)] -> m (Maybe x)
composeLNothings = foldr1 composeNothings

