{-# LANGUAGE OverloadedStrings #-}
module Day8(day8a,day8b) where

import           Control.Monad             (replicateM)
import qualified Data.Attoparsec.Text.Lazy as P
import           Types
import           Utils

day8a :: Problem
day8a = problemSimple "8A" "8" $ sumMetadata . unsafeParse nodeParser
day8b :: Problem
day8b = problemSimple "8B" "8" $ value . unsafeParse nodeParser

data Node = Node [Node] [Int]

sumMetadata :: Node -> Int
sumMetadata (Node c m) = sum (map sumMetadata c ++ m)

value :: Node -> Int
value (Node [] ms) = sum ms
value (Node cs ms) = sum $ map value goodReferences
  where
    cLen = length cs
    goodReferences = map (\x -> cs!!(x-1)) . filter (\x -> x > 0 && x <= cLen) $ ms

nodeParser :: P.Parser Node
nodeParser = do
    P.skipSpace
    cCount <- P.decimal
    P.skipSpace
    mCount <- P.decimal
    P.skipSpace
    cs <- replicateM cCount nodeParser
    P.skipSpace
    ms <- replicateM mCount (P.decimal <* P.skipSpace)
    return $ Node cs ms
