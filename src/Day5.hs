module Day5(day5a,day5b) where

import qualified Data.Text.Lazy as TL
import Types
import Data.Char(toLower)
import Data.Ord(comparing)
import Data.List(minimumBy)

day5a :: Problem
day5a = problemSimple "5A" "5" process
day5b :: Problem
day5b = problemSimple "5B" "5" process'

match :: Char -> Char -> Bool
match a b = toLower a == toLower b && a /= b

minimise :: String -> String
minimise = go []
  where
    go as [] = as -- this returns reversed, but we don't really care.
    go [] (a:as) = go [a] as
    go (a:as) (b:bs) 
        | match a b = go as bs
        | otherwise = go (b:a:as) bs

process :: TL.Text -> Int
process t = length minimised
  where
    minimised = minimise . TL.unpack . TL.strip $ t

process' :: TL.Text -> (Char,Int)
process' t = minimumBy (comparing snd) mins
  where
    str = TL.unpack . TL.strip $ t
    preShrink = minimise str
    mins = [(c,length . minimise . filter ((/=c).toLower) $ preShrink) | c <- ['a'..'z']]
  