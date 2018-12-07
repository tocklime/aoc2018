{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module Day7(day7a,day7b) where

import Control.Arrow((&&&))
import qualified Data.Attoparsec.Text.Lazy as P
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import qualified Data.List as L
import           Types
import           Utils
import Data.Char(ord)

import           Control.Applicative       (many)
day7a :: Problem
day7a = problemSimple "7A" "7" $ solvePuzzle . mkPrereqs . unsafeParse (many lineParser)
day7b :: Problem
day7b = problemSimple "7B" "7" $ scheduleWork . mkPrereqs . unsafeParse (many lineParser)

type Prereq = (Char,S.Set Char)
lineParser :: P.Parser Prereq
lineParser = do
    P.skipSpace
    _ <- P.string "Step "
    a <- P.anyChar
    _ <- P.string " must be finished before step "
    b <- P.anyChar
    _ <- P.string " can begin."
    return (b,S.singleton a)

mkPrereqs :: [Prereq] -> M.Map Char (S.Set Char)
mkPrereqs ls = M.fromListWith S.union $ ls ++ allMentioned
  where
    allMentioned = map (,S.empty) . S.toList . S.unions . map snd $ ls

solvePuzzle :: M.Map Char (S.Set Char) -> String
solvePuzzle m
  | M.null m = []
  | otherwise = case removeFirstEmpty m of
      Just (x,m') -> x : solvePuzzle m'
      Nothing     -> error "Circular dependencies"

markDone :: M.Map Char (S.Set Char) -> Char -> M.Map Char (S.Set Char)
markDone m c = M.map (S.delete c) . M.delete c $ m

markManyDone :: String -> M.Map Char (S.Set Char) -> M.Map Char (S.Set Char)
markManyDone cs m = L.foldl' markDone m cs

removeFirstEmpty :: M.Map Char (S.Set Char) -> Maybe (Char,M.Map Char (S.Set Char))
removeFirstEmpty m = case mFirstEmpty of
    Nothing    -> Nothing
    Just (c,_) -> Just (c, markDone m c)
  where
    mFirstEmpty = M.lookupMin $ M.filter S.null m

allDoable :: M.Map Char (S.Set Char) -> String
allDoable = M.keys . M.filter S.null

duration :: Char -> Int
duration c = 61 + (ord c - ord 'A' )

workers :: Int
workers = 5

scheduleWork :: M.Map Char (S.Set Char) -> Int
scheduleWork = go 0 []
  where
    go time currentWork m 
      | null work = time
      | otherwise = go (time+1) work newMap
      where
        newMap = markManyDone (map fst finished) m
        (finished,ongoing) = L.partition ((<=time) . snd) currentWork
        availableWorkers = workers - length ongoing
        ongoingChars = map fst ongoing
        newWork = map (id&&&(+time) . duration) . take availableWorkers . filter (`notElem` ongoingChars) $ allDoable newMap
        work = ongoing ++ newWork

