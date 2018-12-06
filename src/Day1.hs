module Day1 (day1b,day1a) where

import           Data.Conduit             ((.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text        as CT
import           Types
import Utils

day1a :: Problem
day1a = problemConduit "1A" "1" $ CT.lines .| C.map readInt .| C.sum

day1b :: Problem
day1b = problemConduit "1B" "1" $ CT.lines .| C.map readInt .| repeatConduit .| C.scanl (+) 0 .| findDuplicate
