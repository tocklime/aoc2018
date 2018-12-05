module Day1 (day1b,day1a) where

import           Data.Conduit             ((.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text        as CT
import           Types
import Utils

day1a :: Problem IO
day1a = Problem "1A" "1" $ CT.lines .| C.map readInt .| C.sum

day1b :: Problem IO
day1b = Problem "1B" "1" $ CT.lines .| C.map readInt .| repeatConduit .| C.scanl (+) 0 .| findDuplicate
