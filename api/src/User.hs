{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module User where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Config (ConfigM, Config (pool, gen))
import Control.Monad.Cont (MonadIO(liftIO), guard)
import qualified Crypto.BCrypt as C
import Data.ByteString.Char8
import Data.Maybe (fromJust)
import Data.Functor
import Data.Time
import Crypto.Random (CryptoRandomGen(genBytes))
import Data.IORef (readIORef, writeIORef)
import Control.Monad.RWS (asks)
import Data.ByteString.Base64.URL (encodeBase64, encodeBase64')
import qualified Data.ByteString as Data.Text
import Data.Text.Lazy

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username String
    password String
    UniqueUsername username
    deriving Show
Solve
    millis Int
    userId UserId
    deriving Show
LoginToken
    token String
    ownerId UserId
    creationTimestamp UTCTime default=CURRENT_TIME
    UniqueToken token
    deriving Show
|]

p :: String -> ByteString
p  = Data.ByteString.Char8.pack
up :: ByteString -> [Char]
up = Data.ByteString.Char8.unpack

-- We use fromJust, because the function fails only on bad hash settings which shouldn't ever be a problem
hashPassword :: String -> ConfigM String
hashPassword s = up . fromJust <$> (liftIO . C.hashPasswordUsingPolicy C.slowerBcryptHashingPolicy . p $ s)

validatePassword :: String -> String -> Bool
validatePassword hashedOriginal plaintextTest = C.validatePassword (p hashedOriginal) (p plaintextTest)

-- Helper function that runs sql command
runDBcommand :: SqlPersistT IO a -> ConfigM a
runDBcommand q = asks pool >>= liftIO . runSqlPool q

setupMigrations :: ConfigM ()
setupMigrations = runDBcommand $ runMigration migrateAll


-- TODO error handling
getUserByUsername :: String -> ConfigM (Maybe (Entity User))
getUserByUsername = runDBcommand . getBy . UniqueUsername

validateUserAndGet :: String -> String -> ConfigM (Maybe (Entity User))
validateUserAndGet username password = do
    maybeUser0 <- getUserByUsername username
    return $ do
        Entity userId user <- maybeUser0
        if validatePassword (userPassword user) password
            then Just (Entity userId user)
            else Nothing

-- createUser :: User -> ... would've been fine too
createUser :: String -> String -> ConfigM (Key User)
createUser username password = do
    hashedPassword <- hashPassword password
    runDBcommand . insert $ User username hashedPassword

addTimeForUser :: Int -> Entity User -> ConfigM (Key Solve)
addTimeForUser millis (Entity userId _) = runDBcommand . insert $ Solve millis userId

getSolvesForUser :: Entity User -> ConfigM [Entity Solve]
getSolvesForUser (Entity userId _) = runDBcommand $ selectList [SolveUserId ==. userId] []

-- This function doesn't account for erronnous values and will crash
-- TODO move to util lib
forceRight :: Either a b -> b
forceRight (Right b) = b
forceRight (Left _)  = error "Can't force right in forceRight"

-- TODO Add the number of bytes into config
genLoginToken :: ConfigM String
genLoginToken = do
    (tokenBs, g) <- (asks gen >>= liftIO . readIORef) <&> (forceRight . genBytes 16)
    asks gen >>= liftIO . flip writeIORef g
    return $ up . encodeBase64' $ tokenBs

now :: ConfigM UTCTime
now = liftIO getCurrentTime

createLoginToken :: Entity User -> ConfigM (Key LoginToken)
createLoginToken (Entity userId _) = do
    token <- genLoginToken
    timestamp <- now
    runDBcommand . insert $ LoginToken token userId timestamp

getLoginTokenByKey :: Key LoginToken -> ConfigM (Maybe LoginToken)
getLoginTokenByKey = runDBcommand . get


createLoginTokenAndGet :: Entity User -> ConfigM LoginToken
createLoginTokenAndGet a = fromJust <$> (getLoginTokenByKey =<< createLoginToken a)

createLoginTokenAndGetToken :: Entity User -> ConfigM String
createLoginTokenAndGetToken a = loginTokenToken . fromJust <$> (getLoginTokenByKey =<< createLoginToken a)

validateRawToken :: String -> ConfigM (Maybe (Entity User))
validateRawToken token = do
    maybeLoginToken <- runDBcommand . getBy $ UniqueToken token
    case maybeLoginToken of
        Just (Entity _ loginToken) ->
            if loginTokenToken loginToken == token
                then do
                    maybeUser <- runDBcommand . get $ loginTokenOwnerId loginToken
                    case maybeUser of
                        Nothing -> return Nothing
                        Just user -> return $ Just (Entity (loginTokenOwnerId loginToken) user)
                else return Nothing
        Nothing -> return Nothing