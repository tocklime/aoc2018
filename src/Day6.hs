{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Day6(day6a,day6b) where

import           Control.Arrow             ((&&&))
import qualified Data.Attoparsec.Text.Lazy as P
import           Data.Function             (on)
import           Data.List                 (groupBy, maximumBy, nub, sort,
                                            sortBy)
import           Data.Maybe                (isJust)
import           Data.Ord                  (comparing)
import qualified Data.Sequence             as Seq
import qualified Data.Set                  as S
import           Types
import           Utils

import           Control.Applicative       (many)
day6a :: Problem
day6a = problemSimple "6A" "6" (biggestRegion . unsafeParse (many lineParser))
day6b :: Problem
day6b = problemSimple "6B" "6" (nearMost . unsafeParse (many lineParser))

type Coord = (Int,Int)
type Input = [Coord]
lineParser :: P.Parser Coord
lineParser = do
    P.skipSpace
    a <- P.decimal
    _ <- P.string ", "
    b <- P.decimal
    return (a,b)

uniqueMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
uniqueMinimumBy f as = case sortBy f as of
    []      -> Nothing
    [x]     -> Just x
    (x:y:_) -> if f x y == EQ then Nothing else Just x

biggestRegion :: [Coord] -> (Coord,Int)
biggestRegion is = grouped
  where
    (mx,my) = gridSize is
    coords = [(x,y) | x <- [0..mx+1], y <- [0..my+1]]
    maybeClosest c = uniqueMinimumBy (comparing (manhattanDistance c)) is
    dists = [(x,c) | c <- coords, let maybex = maybeClosest c, isJust maybex, let Just x = maybex]
    edges = filter (isEdge . snd) dists
    infinites = nub . sort . map fst $ edges
    grouped = maximumBy (comparing snd) . filter ((`notElem` infinites) . fst) . map (fst . head &&& length) . groupBy ((==) `on` fst) . sortBy (comparing fst) $ dists
    isEdge (x,y) = x == 0 || y == 0 || x == mx+1 || y == my+1

gridSize :: Input -> (Int,Int)
gridSize is = (x,y)
  where
    x = maximum . map fst $ is
    y = maximum . map snd $ is

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (a,b) (c,d) = abs (c-a) + abs (d-b)

totalDistance :: [Coord] -> Coord -> Int
totalDistance points c = sum $ map (manhattanDistance c) points

listMiddle :: Enum a => [a] -> [a]
listMiddle x = 
    if even (l `mod` 2) 
        then [Seq.index s (l `div` 2)]
        else [Seq.index s (l`div`2) .. Seq.index s (1 + (l`div`2))]
  where
    s = Seq.fromList x
    l = Seq.length s

nearMost :: [Coord] -> Int
nearMost is = S.size (expand initialSet initialSet)
  where
    xMid = listMiddle . sort . map fst $ is
    yMid = listMiddle . sort . map snd $ is
    initialSet :: S.Set Coord
    initialSet = S.fromList [(x,y) | x <- xMid, y <- yMid]
    expand :: S.Set Coord -> S.Set Coord -> S.Set Coord
    expand inArea fringe = if S.null new then inArea else expand (S.union new inArea) new
      where
        candidates :: S.Set Coord
        candidates = S.unions $ S.toList $ S.map neighbours fringe
        new :: S.Set Coord
        new = S.filter ((<10000) . totalDistance is) $ S.difference candidates inArea
        neighbours :: Coord -> S.Set Coord
        neighbours (x,y) = S.fromList [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
