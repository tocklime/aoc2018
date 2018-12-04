{-# LANGUAGE ExistentialQuantification #-}
module Types where

import           Control.Monad.Trans          (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (MonadThrow, MonadUnliftIO,
                                               ResourceT)
import           Data.Conduit                 (ConduitT, Void, runConduitRes,
                                               (.|))
import           Data.Conduit.Binary          (sourceFile)
import           Data.Conduit.Combinators     (decodeUtf8, stdin)
import           Data.Text                    (Text)

data Problem m = forall o. (Show o) => Problem
    { problemName :: String
    , defaultFile :: String
    , solve       :: ConduitT Text Void (ResourceT m) o
    }

data RunMode = Stdin | DefaultFile | SpecificFile FilePath

runProblem :: (MonadIO m,MonadUnliftIO m,MonadThrow m) => RunMode -> Problem m -> m ()
runProblem t (Problem _ f s) = do
    out <- runConduitRes $ sourceThing .| decodeUtf8 .| s
    liftIO $ print out
    where
        sourceThing = case t of
            Stdin           -> stdin
            DefaultFile     -> sourceFile ("inputs/Day"++f++".txt")
            SpecificFile fi -> sourceFile fi
