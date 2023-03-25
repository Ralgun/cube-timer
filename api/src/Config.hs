{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where
import Database.Persist.Sql (ConnectionPool)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader, asks, MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.Generics (Generic)
import Data.Functor
import Crypto.Random (CryptoRandomGen, SystemRandom)
import Data.IORef (IORef)

data Config = Config
    { pool :: ConnectionPool
    , gen  :: IORef SystemRandom
    }

newtype ConfigM a = ConfigM
    { runConfigM :: ReaderT Config IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config) -- MonadReader basically allows me to use asks in ConfigMonad