{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api where

import Data.Aeson
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.Generics (Generic)
import Database.Persist.Sqlite (withSqlitePool, Entity (Entity), ConnectionPool, createSqlitePool)
import User (getSolvesForUser, getUserByUsername, Solve (Solve), setupMigrations, addTimeForUser, createUser, validateUserAndGet, createLoginToken, createLoginTokenAndGet, createLoginTokenAndGetToken, validateRawToken, User (userUsername))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Functor
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader, asks, MonadTrans (lift), guard)
import Web.Scotty.Internal.Types (ScottyT, ActionT)

-- *** We must use the .Trans versions to be able to move across transformers
import Web.Scotty.Trans (scottyOptsT, scottyT, middleware, text, json, get, post, html, notFound, param, jsonData, raiseStatus)
-- ***
import Web.Scotty.Cookie

import Data.Text.Lazy (Text, toStrict, pack, unpack)
import Config (ConfigM(runConfigM), Config (Config, pool))
import Crypto.Random (CryptoRandomGen(newGenIO))
import Data.IORef (newIORef)
import Control.Monad.Trans
import Web.Scotty.Cookie (setSimpleCookie)
import Data.Text (unpack)
import qualified Control.Applicative as Data.Applicative
import qualified Data.Maybe

type Error = Text

data TimeForm = TimeForm { tMillis :: Int } deriving ( Show, Generic )
data UserForm = UserForm { uUsername :: String, uPassword :: String } deriving (Show, Generic)


instance FromJSON TimeForm
instance FromJSON UserForm

-- TODO Don't store connection string in plaintext
createConnectionPool :: IO ConnectionPool
createConnectionPool = runStdoutLoggingT $ createSqlitePool "../db/cube-timer-test.db" 4

api :: IO ()
api = do
    -- Create config that we will feed into ConfigMonad context
    p <- createConnectionPool
    r <- newGenIO >>= newIORef
    let c = Config p r

    -- Otherwise the db won't work
    -- We basically use runConfigM to get the reader and then we feed it the environment
    runReaderT (runConfigM setupMigrations) c

    -- This is basically a recipe that says how to turn ConfigMonad into an IO monad
    -- r :: ConfigM a -> IO a
    let r m = runReaderT (runConfigM m) c
        app = application c
    scottyT 4000 r app


type Action = ActionT Error ConfigM ()

application :: Config -> ScottyT Error ConfigM ()
application c = do
    get "/api/:username/times" $ do
        maybeUser <- param "username" >>= lift . getUserByUsername
        solveEntities <-
            case maybeUser of
                Nothing -> return []
                Just u  -> lift $ getSolvesForUser u
        let solves = map (\(Entity _ (Solve millis _)) -> millis) solveEntities
        Web.Scotty.Trans.json solves
    post "/api/create-user" $ do
        UserForm u p <- jsonData @UserForm
        lift $ createUser u p
        text "ok" -- TODO this is not how you do it
    post "/api/login" $ do
        UserForm username password <- jsonData @UserForm
        liftIO $ putStrLn "AAA"
        maybeUserEntity <- lift $ validateUserAndGet username password
        liftIO $ putStrLn . show $ maybeUserEntity
        text =<< case maybeUserEntity of
            Just u  -> do
                token <- lift $ createLoginTokenAndGetToken u
                -- TODO make httpOnly, see below
                -- https://hackage.haskell.org/package/cookie-0.4.1.4/docs/Web-Cookie.html#t:SetCookie
                setSimpleCookie "login_token" (Data.Text.Lazy.toStrict . pack $ token)
                return "ok"
            Nothing -> return "fail"
    post "/api/send-time" $ do
        maybeToken <- getCookie "login_token"
        text =<< case maybeToken of
            Nothing         -> return "Unauth"
            Just cookieText -> do
                maybeUserEntity <- lift . validateRawToken . Data.Text.unpack $ cookieText
                case maybeUserEntity of
                    Nothing -> return "Unauth"
                    Just userEntity -> do
                        timeForm <- jsonData @TimeForm
                        lift (addTimeForUser (tMillis timeForm) userEntity) >> return "ok"
    get "/api/user/me" $ do
        maybeToken <- getCookie "login_token"
        text =<< case maybeToken of
            Nothing         -> return "Unauth"
            Just cookieText -> do
                maybeUserEntity <- lift . validateRawToken . Data.Text.unpack $ cookieText
                case maybeUserEntity of
                    Nothing -> return "Unauth"
                    Just (Entity _ user) -> return . pack . userUsername $ user

    notFound $ text "HOLY FUCKING SHIT HOLY FUCK OH GOD OH NO"