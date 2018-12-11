{-# LANGUAGE OverloadedStrings #-}
module Day10(day10) where

import           Control.Monad             (replicateM)
import qualified Data.Attoparsec.Text.Lazy as P
import           Types
import           Utils
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Applicative(many)

day10 :: Problem
day10 = problemText "10A" "10" $ findSmallest . unsafeParse (many starParser <* P.endOfInput)

data Star = Star 
  {  px0 :: Int
  ,  py0 :: Int
  ,  vx :: Int
  , vy :: Int
  } deriving(Show)

posAt :: Int -> Star -> (Int,Int)
posAt n (Star x y vx vy) = (x+n*vx,y+n*vy)


findSmallest :: [Star] -> String
findSmallest stars = show i ++ "\n" ++ drawAt i stars
  where
    i = localMinimumIx1 (`areaAt` stars) [0..]
drawMany :: Int -> [Star] -> String
drawMany max stars = unlines ["\n\n"++show n ++ "\n"++drawAt n stars| n <- [0..max]]

localMinimumIx1 :: Ord o => (a -> o) ->[a] -> Int
localMinimumIx1 f (a:as) = go 0 a as
  where
    go ix last [] = ix
    go ix last (b:bs)
      | f last < f b = ix
      | otherwise = go (ix+1) b bs

sizeAt :: Int -> [Star] -> ((Int,Int),(Int,Int))
sizeAt n stars = ((minX,minY),(maxX,maxY))
  where
    poss = S.fromList $ map (posAt n) stars
    (minX,_) = S.findMin poss
    (maxX,_) = S.findMax poss
    minY = minimum $ S.map snd poss
    maxY = maximum $ S.map snd poss

areaAt :: Int -> [Star] -> Int
areaAt n stars = (maxX-minX) * (maxY - minY)
  where
    poss = S.fromList $ map (posAt n) stars
    ((minX,minY),(maxX,maxY)) = sizeAt n stars


drawAt :: Int -> [Star] -> String
drawAt n stars = unlines [[if s then '#' else '.' | x <- [minX..maxX], let s = S.member (x,y) poss] | y <- [minY..maxY]]
  where
    poss = S.fromList $ map (posAt n) stars
    ((minX,minY),(maxX,maxY)) = sizeAt n stars

starParser :: P.Parser Star
starParser = do
    P.skipSpace
    _ <- P.string "position=<"
    P.skipSpace
    x0 <- P.signed P.decimal
    _ <- P.string ","
    P.skipSpace
    y0 <- P.signed P.decimal
    _ <- P.string "> velocity=<"
    P.skipSpace
    velX <- P.signed P.decimal
    _ <- P.string ","
    P.skipSpace
    velY <- P.signed P.decimal
    _ <- P.string ">"
    P.skipSpace
    return $ Star x0 y0 velX velY
