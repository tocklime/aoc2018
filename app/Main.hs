module Main where

import           Control.Monad (forM_)
import           Day1
import           Day10
import           Day11
import           Day12
import           Day13
import Day14
import           Day2
import           Day3
import           Day4
import           Day5
import           Day6
import           Day7
import           Day8
import           Day9
import           Types

today :: [Problem]
today = [day14a,day14b]

allprobs :: [Problem]
allprobs = [day1a,day1b,day2a,day2b,day3a,day3b,day4a,day4b,day5a,day5b,day6a,day6b,day7a,day7b,day8a,day8b,day9a,day9b,day10,day11a,day11b,day12a,day12b,day13]

main :: IO ()
main = forM_ today $ \p -> do
    putStrLn $"Doing problem " ++ problemName p
    runProblem DefaultFile p
