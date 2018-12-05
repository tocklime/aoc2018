{-# LANGUAGE OverloadedStrings #-}
module Day3(day3a,day3a',day3b) where

import Types
import Control.Concurrent.MVar
import qualified Data.Array.MArray as MA
import qualified Data.Array.IO as AIO
import qualified Data.Array as A
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text        as CT
import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad(forM_,forM,when)
import Control.Monad.Fail(MonadFail)
import Control.Monad.Trans(liftIO,MonadIO)
import Control.Monad.Trans.Resource
import qualified Data.Attoparsec.Text as P
data Claim = Claim 
    { _id :: Int
    , left :: Int
    , top :: Int
    , width :: Int
    , height :: Int
    } deriving(Show,Eq,Ord)

day3a' :: Problem IO
day3a' = Problem "3A" "3" $ CT.lines .| C.mapM readLine .| C.head
day3a :: Problem IO
day3a = Problem "3A" "3" $ CT.lines .| C.mapM readLine .| collapseAndCount
day3b :: Problem IO
day3b = Problem "3B" "3" $ CT.lines .| C.mapM readLine .| collapseAndFindDisconnected

lineParse :: P.Parser Claim
lineParse = do -- #{id} @ {l},{t}: {w}x{h}
  _ <- P.string "#"
  i <- P.decimal
  _ <- P.string " @ "
  l <- P.decimal
  _ <- P.string ","
  t <- P.decimal
  _ <- P.string ": "
  w <- P.decimal
  _ <- P.string "x"
  h <- P.decimal
  return $ Claim i l t w h


readLine :: (MonadFail m) => T.Text -> m Claim
readLine t = handle (P.parse lineParse t)
  where 
    handle (P.Done _ r) = return r
    handle (P.Fail _ _ e) = fail e
    handle (P.Partial f) = handle $ f ""


type Sheet a = AIO.IOArray (Int,Int) a

collapseAndCount :: ConduitT Claim o (ResourceT IO) Int
collapseAndCount = do
    sheet <- liftIO $ (MA.newArray ((0,0),(1000,1000)) (0) :: IO (Sheet Int))
    C.mapM_ $ \c -> do
        forM_ [(x,y)| x <-[left c..left c+(width c - 1)], y <- [top c..top c+(height c - 1)]] $ \p -> do
            count <- liftIO $ MA.readArray sheet p
            liftIO $ MA.writeArray sheet p (count+1)
    frozen <- liftIO $ MA.freeze sheet
    return . length . filter (>1) . A.elems $ frozen
    
collapseAndFindDisconnected :: MonadIO m => ConduitT Claim o m [Claim]
collapseAndFindDisconnected = do
    sheet <- liftIO $ (MA.newArray ((0,0),(1000,1000)) [] :: IO (Sheet [Claim]))
    alones <- liftIO $ newMVar S.empty
    C.mapM_ $ \c -> do
        newness <- forM [(x,y)| x <-[left c..left c+(width c - 1)], y <- [top c..top c+(height c - 1)]] $ \p -> do
            claims <- liftIO $ MA.readArray sheet p
            forM_ claims $ \otherclaim ->
                liftIO $ modifyMVar_ alones $ \set -> return $ S.delete otherclaim set
            liftIO $ MA.writeArray sheet p (c:claims)
            return (null claims)
        let isNew = all id newness
        when isNew $ do
            liftIO $ modifyMVar_ alones $ \set -> return $ S.insert c set
    c <- liftIO $ readMVar alones
    return (S.toList c)
