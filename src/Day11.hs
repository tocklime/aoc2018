{-# LANGUAGE OverloadedStrings #-}
module Day11(day11a,day11b) where

import           Control.Monad
import qualified Data.Array    as A
import           Data.Array.ST
import           Data.List     (maximumBy)
import           Data.Ord      (comparing)
import           Types

day11a :: Problem
day11a = problemText "11A" "11" $ const (show . best $ sumsOfSize 3)
day11b :: Problem
day11b = problemText "11B" "11" $ const (show . best $ allSums)

onlyHundreds :: Int -> Int
onlyHundreds x = (x `mod` 1000) `div` 100

serial :: Int
serial = 1718

powerLevel :: (Int,Int) -> Int
powerLevel (x,y) = subtract 5 . onlyHundreds . (*(x+10)) . (+serial) . (*y) . (+10) $ x

prefixSumGrid :: ((Int,Int) -> Int) -> A.Array (Int, Int) Int
prefixSumGrid f = runSTArray $ do
    arr <- newArray ((1,1),(301,301)) 0
    forM_ [(x,y)| x <- [1..301],y <- [1..301]] $ \(x,y) -> do
        up <- if y > 1 then readArray arr (x,y-1) else pure 0
        left <- if x > 1 then readArray arr (x-1,y) else pure 0
        upleft <- if x>1 && y > 1 then readArray arr (x-1,y-1) else pure 0
        writeArray arr (x,y) (f (x,y) + up + left - upleft)
    return arr

neighbourhoodSum :: Int -> (Int,Int) -> Int
neighbourhoodSum s (a,b) = upleft + bottomright - left - up
  where
    upleft = if a > 1 && b > 1 then preGrid A.! (a-1,b-1) else 0
    bottomright = preGrid A.! (a+s-1,b+s-1)
    left = if a > 1 then  preGrid A.! (a-1,b+s-1) else 0
    up = if b > 1 then preGrid A.! (a+s-1,b-1) else 0

best :: Ord a => [(a,b)] -> (a,b)
best = maximumBy (comparing fst)

preGrid :: A.Array (Int, Int) Int
preGrid = prefixSumGrid powerLevel

sumsOfSize :: Int -> [(Int,(Int,Int))]
sumsOfSize n = [(neighbourhoodSum n (x,y),(x,y))|x<-[1..300 - n],y<-[1..300 - n]]

allSums :: [(Int,(Int,Int,Int))]
allSums = [(b,(x,y,s)) | s <- [299,298..1], let (b,(x,y)) = best $ sumsOfSize s]
