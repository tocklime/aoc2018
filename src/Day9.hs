{-# LANGUAGE OverloadedStrings #-}
module Day9(day9a,day9b) where

import           Types
import qualified Data.Sequence as S
import Data.Sequence ((<|),(><),ViewL(..))
import qualified Data.Map.Strict as M

day9a :: Problem
day9a = problemSimple "9A" "9" $ const (winner $ marbles 71522 446)
day9b :: Problem
day9b = problemSimple "9B" "9" $ const (winner $ marbles 7152200 446) 

splitAtCycle :: Int -> S.Seq a -> (S.Seq a, S.Seq a)
splitAtCycle n s = S.splitAt (n `mod` S.length s) s

winner :: M.Map Int Int -> Int
winner = maximum . M.elems

marbles :: Int -> Int -> M.Map Int Int
marbles turnLimit nPlayers = play (S.singleton 0) 0 (M.fromList [(n,0)|n <- [0..nPlayers-1]])
  where
    play board currentTurn scores 
      | currMarble > turnLimit = scores
      | currMarble `mod` 23 == 0 = play takenBoard (currentTurn+1) newScores
      | otherwise = play newBoard (currentTurn+1) scores
      where
        currMarble = currentTurn+1
        currPlayer = currentTurn `mod` nPlayers
        (l,r) = splitAtCycle 2 board
        newBoard = (currentTurn+1) <| r >< l
        (sl,sr) = splitAtCycle (S.length board - 7) board
        (t :< sr') = S.viewl sr
        takenBoard = sr' >< sl
        newScores = M.adjust ((+t).(+currMarble)) currPlayer scores