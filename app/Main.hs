module Main where

import           Control.Monad (forM_)
import           Day1
import           Day2
import           Day3
import           Day4
import           Types

probs :: [Problem IO]
probs = [day1a,day1b,day2a,day2b,day3a,day3b,day4a,day4b]

main :: IO ()
main = forM_ probs $ \p -> do
    putStrLn $"Doing problem " ++ problemName p
    runProblem DefaultFile p
