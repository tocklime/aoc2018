{-# LANGUAGE OverloadedStrings #-}
module Day4(day4a,day4b) where

import           Control.Applicative
import Control.Monad.Trans.Resource
import qualified Data.Attoparsec.Text as P
import           Data.Conduit
import Data.Ord(comparing)
import qualified Data.Conduit.List    as CL
import qualified Data.Conduit.Text    as CT
import           Data.List            ( sort,scanl',group,maximumBy)
import qualified Data.Map.Strict      as M
import Data.Map.Strict ((!))
import qualified Data.Text            as T
import           Types
import           Utils

day4a :: Problem
day4a = problemConduit "4A" "4" $ CT.lines .| processLines solvePartA
day4b :: Problem
day4b = problemConduit "4B" "4" $ CT.lines .| processLines solvePartB

data Shift = Shift
    { guardId :: Integer
    , naps    :: [(Integer,Integer)]
    } deriving (Show,Eq)

data Line = GuardStart Integer | FallAsleep Integer | Wake Integer

lineParse :: P.Parser Line
lineParse = do -- [YYYY-mm-dd hh:mm] {Guard #id begins shift,falls asleep, wakes up}
  _ <- P.string "["
  _y <- int
  _ <- P.string "-"
  _m <- int
  _ <- P.string "-"
  _d <- int
  _ <- P.string " "
  _h <- int
  _ <- P.string ":"
  minute <- int
  _ <- P.string "] "
  guard <|> asleep minute <|> wake minute
  where
    int :: P.Parser Integer
    int = P.decimal
    guard = do
        _ <- P.string "Guard #"
        i <- int
        _ <- P.string " begins shift"
        return (GuardStart i)
    asleep x = P.string "falls asleep" >> return (FallAsleep x)
    wake x = P.string "wakes up" >> return (Wake x)


solvePartA :: [Shift] -> Integer
solvePartA shifts = sleepiestGuard * minute
  where
    bg = byGuard shifts
    sleepiestGuard = fst . maximumBy (comparing (totalAsleep . snd)) . M.toList $ bg
    gNaps = bg ! sleepiestGuard
    runningSleeps = scanl' running (0,0) . compress . sort . concatMap (\(a,b) -> [(a,1),(b,-1)]) $ gNaps
    minute = fst . maximumBy (comparing snd) $ runningSleeps
    running :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
    running (_,totSleeps) (a,b) = (a,b+totSleeps) 
    compress [] = []
    compress [a] = [a]
    compress ((a,b):(c,d):rest)
        | a == c && b + d == 0 = compress rest
        | a == c = compress ((a,b+d):rest)
        | otherwise = (a,b):compress ((c,d):rest)


solvePartB :: [Shift] -> Integer
solvePartB shifts = uncurry (*) . head . maximumBy (comparing length) . group . sort $ [(m,guard) | (Shift guard ns) <- shifts, (a,b) <- ns, m <- [a..b-1]]

processLines :: ([Shift] -> Integer) -> ConduitT T.Text Void (ResourceT IO) Integer
processLines fn = do
    vals <- CL.consume
    let sorted = sort vals
    parsed <- mapM (doParse lineParse) sorted
    let shifts = toShifts parsed
    return $ fn shifts

totalAsleep :: [(Integer,Integer)] -> Integer
totalAsleep = sum . map (\(a,b) -> b-a)


byGuard :: [Shift] -> M.Map Integer [(Integer,Integer)]
byGuard ss = M.fromListWith (++) [(i,ns) | (Shift i ns) <- ss]

toShifts :: [Line] -> [Shift]
toShifts ls = map mkShift brokenUp
  where
    isStart (GuardStart _) = True
    isStart _ = False
    brokenUp = breakOn isStart ls
    mkShift (GuardStart i:rest) = Shift i (map mkNap $ asPairs rest)
    mkShift _ = error "Shift doesn't start with Guard"
    mkNap (FallAsleep x,Wake y) = (x,y)
    mkNap _ = (0,0)
    