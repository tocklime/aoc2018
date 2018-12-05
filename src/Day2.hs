{-# LANGUAGE OverloadedStrings #-}
module Day2(day2a,day2b) where

import           Data.Bool                (bool)
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text        as CT
import           Data.List                (group, sort)
import           Data.Text                (Text, unpack)
import qualified Data.Text as T
import           Types
import Utils

data Line = Line Int Int

instance Show Line where
    show (Line a b) = show (a*b)
instance Semigroup Line where
    (Line a b) <> (Line c d) = Line (a+c) (b+d)
instance Monoid Line where
    mempty = Line 0 0

day2a :: Problem IO
day2a = Problem "2A" "2" $ CT.lines .| C.map analyseLine .| C.fold

analyseLine :: Text -> Line
analyseLine = (\x -> Line (has 2 x) (has 3 x)) . map length . group . sort . unpack
  where
    has n x = bool 0 1 (n `elem` x)

blankNth :: Text -> Int -> Text
blankNth t n = let (a,b) = T.splitAt n t in a <> "_" <> T.tail b

day2b :: Problem IO
day2b = Problem "2B" "2" $ CT.lines .| C.concatMap expand .| findDuplicate
  where
    expand :: Text -> [Text]
    expand t = map (blankNth t) [0..T.length t - 1] 
