{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Day12(day12a,day12b) where

import qualified Data.Attoparsec.Text.Lazy as P
import qualified Data.Map.Strict           as M
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           Types
import           Utils

day12a :: Problem
day12a = problemText "12A" "12" $ show . score . nstep 20 . unsafeParse (problemParse <* P.endOfInput)
day12b :: Problem
day12b = problemText "12B" "12" $ show . solvePart2 . last . part2 . unsafeParse (problemParse <* P.endOfInput)

score :: PlantPots -> Integer
score = fromIntegral . sum . S.toList . state

solvePart2 :: (Integer,Integer,Integer) -> Integer
solvePart2 (steps,scoreSoFar,diffPerStep) = scoreSoFar + diffPerStep * (50000000000 - fromIntegral steps)

part2 :: PlantPots -> [(Integer,Integer,Integer)]
part2 = go 0 0 0
  where
    go :: Integer -> Integer -> Integer -> PlantPots -> [(Integer,Integer,Integer)]
    go n nIdenticalDiffs lastDiff p
      | nIdenticalDiffs > 10 = []
      | otherwise = (n,sp,thisDiff)  : rest
      where
        p' = step p
        sp = score p
        sp' = score p'
        thisDiff = sp' - sp
        rest = go (n+1) (if thisDiff == lastDiff then nIdenticalDiffs + 1 else 0) thisDiff p'


data PlantPots = PlantPots
  {
      state    :: S.Set Int
      , _rules :: [Bool] -> Bool
  }

nstep :: Integer -> PlantPots -> PlantPots
nstep 0 !p  = p
nstep !n !p = nstep (n-1) (step p)
step :: PlantPots -> PlantPots
step (PlantPots !s !r) = PlantPots newState r
  where
    smallestSet = S.findMin s
    largestSet = S.findMax s
    newState = S.fromList . filter isSet $ [smallestSet-2..largestSet+2]
    isSet n = r $ map (`S.member` s) [n-2..n+2]

hashesParse :: P.Parser [Bool]
hashesParse = do
    s <- P.takeWhile $ P.inClass "#."
    let initial =  (=='#') <$> T.unpack s
    return initial

ruleParse :: P.Parser ((Bool,Bool,Bool,Bool,Bool),Bool)
ruleParse = do
    _ <- P.skipSpace
    [a,b,c,d,e] <- hashesParse
    _ <- P.string " => "
    [r] <- hashesParse
    return ((a,b,c,d,e),r)

problemParse :: P.Parser PlantPots
problemParse = do
    _ <- P.string "initial state: "
    initial <- S.fromList . map fst . filter snd . zip [0..] <$> hashesParse
    rs <- P.many' ruleParse
    let m = M.fromList rs
    let f l = case l of
                [a,b,c,d,e] -> fromMaybe False $ M.lookup (a,b,c,d,e) m
                _           -> False
    _ <- P.skipSpace
    return $ PlantPots initial f
