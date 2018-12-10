{-# LANGUAGE ExistentialQuantification #-}
module Types where

import           Control.Monad.Trans          (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit                 (ConduitT, Void, runConduitRes,
                                               (.|))
import           Data.Conduit.Binary          (sourceFile)
import           Data.Conduit.Combinators     (decodeUtf8, sinkLazy, stdin)
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as TL

data Problem = Problem
    { problemName :: String
    , defaultFile :: String
    , solve       :: RunMode -> IO () --ConduitT Text Void (ResourceT m) o
    }

problemConduit :: forall o. (Show o) => String -> String -> ConduitT Text Void (ResourceT IO) o -> Problem
problemConduit n f c = Problem n f $ \t -> do
    let sourceThing = case t of
            Stdin           -> stdin
            DefaultFile     -> sourceFile ("inputs/Day"++f++".txt")
            SpecificFile fi -> sourceFile fi
    out <- runConduitRes $ sourceThing .| decodeUtf8 .| c
    liftIO $ print out

problemSimple :: forall o. (Show o) => String -> String -> (TL.Text -> o) -> Problem
problemSimple n f tfunc = problemConduit n f $ tfunc <$> sinkLazy

problemText :: String -> String -> (TL.Text -> String) -> Problem
problemText n f tfunc = Problem n f $ \t -> do
    let sourceThing = case t of
            Stdin           -> stdin
            DefaultFile     -> sourceFile ("inputs/Day"++f++".txt")
            SpecificFile fi -> sourceFile fi
    out <- runConduitRes $ sourceThing .| decodeUtf8 .| tfunc <$> sinkLazy
    liftIO $ putStrLn out
tbd :: a -> String
tbd = const "TBD"


data RunMode = Stdin | DefaultFile | SpecificFile FilePath

runProblem :: RunMode -> Problem -> IO ()
runProblem t (Problem _ _ s) = s t
