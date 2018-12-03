module Day1 (day1p1,day1p2) where

import Data.Attoparsec.Text
import Data.Conduit.Attoparsec
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Text
import qualified Data.Text as T
import Control.Monad.Identity
import Control.Monad.Trans.Resource
import System.IO
import Data.Bool(bool)
import Data.Text.Read
import qualified Data.Set as S
import qualified Data.Conduit.Combinators as C

day1p1 :: IO ()
day1p1 = do
    total <- parseFile "inputs/Day1p1.txt"
    putStrLn (show total)

parseFile :: String -> IO Integer
parseFile x = 
    runConduitRes $ sourceFile x .| decodeUtf8 .| Data.Conduit.Text.lines .| C.map readInt .| C.sum

repeatConduit :: (Monad m) => ConduitT a a m ()
repeatConduit = go []
  where 
    go acc = do
        x <- await
        case x of
            Nothing -> C.yieldMany (cycle acc)
            Just v -> go (acc ++ [v])

readInt :: T.Text -> Integer
readInt t = case Data.Text.Read.signed Data.Text.Read.decimal t of 
    Left _ -> 0
    Right (x,_) -> x

findDuplicate :: (Ord a, Eq a, Monad m) => ConduitT a Void m (Maybe a)
findDuplicate = go S.empty
  where
    go seen = do
      x <- await
      case x of
        Nothing -> return Nothing
        Just v -> case v `S.member` seen of
                    True -> return (Just v)
                    False -> go (S.insert v seen)

day1p2 :: IO ()
day1p2 = do 
    info <- runConduitRes $ 
                sourceFile "inputs/Day1p1.txt" .|
                decodeUtf8 .| 
                Data.Conduit.Text.lines .| 
                C.map readInt .| 
                repeatConduit .| 
                C.scanl (+) 0 .| 
                findDuplicate
    putStrLn $ show info