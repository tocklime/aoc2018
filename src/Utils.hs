{-# LANGUAGE OverloadedStrings #-}
module Utils where

import           Data.Conduit             (ConduitT, Void, await)
import qualified Data.Conduit.Combinators as C
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Data.Text.Read           as TR
import qualified Data.Attoparsec.Text as P
import Control.Monad.Fail(MonadFail)


breakOn :: (a -> Bool) -> [a] -> [[a]]
breakOn _ [] = []
breakOn p (x:xs)  
  | p x = let (a,b) = break p xs in (x:a) : breakOn p b
  | otherwise = breakOn p xs

asPairs :: [a] -> [(a,a)]
asPairs [] = []
asPairs [_] = []
asPairs (x:y:xs) = (x,y) : asPairs xs

repeatConduit :: (Monad m) => ConduitT a a m ()
repeatConduit = go []
  where
    go acc = do
        x <- await
        case x of
            Nothing -> C.yieldMany (cycle acc)
            Just v  -> go (acc ++ [v])

readInt :: T.Text -> Integer
readInt t = case TR.signed TR.decimal t of
    Left _      -> 0
    Right (x,_) -> x

doParse :: (MonadFail m) => P.Parser a -> T.Text -> m a
doParse p t = handle (P.parse p t)
  where 
    handle (P.Done _ r) = return r
    handle (P.Fail _ _ e) = fail e
    handle (P.Partial f) = handle $ f ""

findDuplicate :: (Ord a, Eq a, Monad m) => ConduitT a Void m (Maybe a)
findDuplicate = go S.empty
  where
    go seen = do
      x <- await
      case x of
        Nothing -> return Nothing
        Just v -> if v `S.member` seen
                  then return (Just v)
                  else go (S.insert v seen)