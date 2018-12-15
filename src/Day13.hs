{-# LANGUAGE OverloadedStrings #-}
module Day13(day13) where

import           Control.Arrow   (first, second)
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import qualified Data.Text.Lazy  as TL
import           Types

day13 :: Problem
day13 = problemText "13" "13" $ run . readGrid

data Track = Horiz | Vert | Inter | CornerSlash | CornerBackslash deriving (Show)
data Direction = North | East | South | West deriving (Show)
data IntersectionChoice = L | Straight | R deriving (Show)
data Cart = Cart
  { cartDirection        :: Direction
  , cartNextIntersection :: IntersectionChoice
  } deriving (Show)
type CartPos = ((Int,Int),Cart)
data Network = Net
  { netGrid  :: M.Map (Int,Int) Track
  , netCarts :: M.Map (Int,Int) Cart
  } deriving (Show)


cartMove :: Network -> CartPos -> (Network,Maybe (Int,Int))
cartMove (Net grid carts) (pos,Cart dir ni) =
  if M.member pos carts
    then if M.member nextPos carts
      then (Net grid (M.delete nextPos . M.delete pos $ carts),Just nextPos)
      else (Net grid (M.insert nextPos (Cart nextDir nextNi) . M.delete pos $ carts),Nothing)
    else (Net grid carts, Nothing) -- cart was already in a crash. Ignore
  where
    nextPos = case dir of
        North -> first pred pos
        South -> first succ pos
        East  -> second succ pos
        West  -> second pred pos
    nextTrack = grid M.! nextPos
    (nextDir,nextNi) =
        case (nextTrack,dir,ni) of
          (CornerSlash,North,_)     -> (East,ni)
          (CornerSlash,East,_)      -> (North,ni)
          (CornerSlash,South,_)     -> (West,ni)
          (CornerSlash,West,_)      -> (South,ni)
          (CornerBackslash,North,_) -> (West,ni)
          (CornerBackslash,East,_)  -> (South,ni)
          (CornerBackslash,South,_) -> (East,ni)
          (CornerBackslash,West,_)  -> (North,ni)
          (Inter,_,Straight)        -> (dir,R)
          (Inter,North,L)           -> (West,Straight)
          (Inter,East,L)            -> (North,Straight)
          (Inter,South,L)           -> (East,Straight)
          (Inter,West,L)            -> (South,Straight)
          (Inter,North,R)           -> (East,L)
          (Inter,East,R)            -> (South,L)
          (Inter,South,R)           -> (West,L)
          (Inter,West,R)            -> (North,L)
          _                         -> (dir,ni)


readCell :: Char -> [(Track, Maybe Cart)]
readCell '-'  = [(Horiz, Nothing)]
readCell '|'  = [(Vert, Nothing)]
readCell '/'  = [(CornerSlash, Nothing)]
readCell '\\' = [(CornerBackslash, Nothing)]
readCell '>'  = [(Horiz, Just $ Cart East L)]
readCell '<'  = [(Horiz, Just $ Cart West L)]
readCell '^'  = [(Vert, Just $ Cart North L)]
readCell 'v'  = [(Vert, Just $ Cart South L)]
readCell '+'  = [(Inter,Nothing)]
readCell _    = []

liftMaybe :: (a,Maybe b) -> Maybe (a,b)
liftMaybe (_,Nothing) = Nothing
liftMaybe (a, Just b) = Just (a,b)

readGrid :: TL.Text -> Network
readGrid t = Net tracks carts
  where
    parsed = [((y,x),tmc) | (y,line) <- zip [0..] $ TL.lines t, (x,c) <- zip [0..] $ TL.unpack line, tmc <- readCell c]
    tracks = M.fromList . map (second fst) $ parsed
    carts = M.fromList . mapMaybe (liftMaybe . second snd) $ parsed

tickNetwork :: Network -> ([String],Network)
tickNetwork n = M.foldlWithKey' doCart ([],n) (netCarts n)
  where
    doCart (logs,net) pos c = case cartMove net (pos,c) of
      (net',Just pos') -> (("Crash at "++show pos'):logs,net')
      (net',Nothing)   -> (logs,net')

run :: Network -> String
run n = case tickNetwork n of
  ([],n') -> run n'
  (logs,n') -> let nc = M.size (netCarts n') in
               unlines logs ++ if nc <= 1
                then "Only one cart remains: " ++ show (netCarts n')
                else run n'
