module Main where

import           Control.Monad (forM_)
import           Day1
import           Types

probs :: [Problem IO]
probs = [day1a,day1b]

main :: IO ()
main = forM_ probs $ \p -> do
    putStrLn $"Doing problem " ++ problemName p
    runProblem DefaultFile p
