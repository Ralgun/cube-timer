{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Api where

import Data.Aeson
import GHC.Generics (Generic)
import Database.Persist.Sqlite (Entity (Entity), ConnectionPool, createSqlitePool)
import User (getSolvesForUser, getUserByUsername, Solve (Solve), setupMigrations, addTimeForUser, createUser, validateUserAndGet, createLoginTokenAndGetToken, validateRawToken, User (userUsername))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (ReaderT(runReaderT))
import Web.Scotty.Internal.Types (ScottyT, ActionT)

-- *** We must use the .Trans versions to be able to move across transformers
import Web.Scotty.Trans (scottyT, json, get, post, notFound, param, jsonData, finish, status, finish)
-- ***
import Web.Scotty.Cookie

import Data.Text.Lazy (Text, toStrict, pack)
import Config (ConfigM(runConfigM), Config (Config))
import Crypto.Random (CryptoRandomGen(newGenIO))
import Data.IORef (newIORef)
import Control.Monad.Trans
import Control.Monad.Trans.Except (runExceptT, except)
import Data.Text (unpack)
import Network.HTTP.Types.Status (Status, status401, status404)
import Data.Aeson.KeyMap (fromList, KeyMap)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT, MaybeT))

type Error = Text

data TimeForm = TimeForm { millis :: Int } deriving ( Show, Generic )
data UserForm = UserForm { username :: String, password :: String } deriving (Show, Generic)


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


type Action = ActionT Error Config.ConfigM ()
type ActionA a = ActionT Error Config.ConfigM a

handleEither :: ActionA (Either RequestError a) -> ActionA a
handleEither a = do
    b <- a
    case b of
        Left e -> jsonSimpleError e
        Right a -> return a

data RequestError = RequestError { errorMessage :: String, statusCode :: Status }
data JsonResponse a = JsonResponse { status :: String, payload :: KeyMap a } deriving (Show, Generic)
instance (ToJSON a) => ToJSON (JsonResponse a)

missingUserError :: RequestError
missingUserError = RequestError "Couldn't find the user" status404
unauthorizedUserError :: RequestError
unauthorizedUserError = RequestError "Unauthorized" status401

jsonSimpleSuccess :: [(Key, String)] -> Action
jsonSimpleSuccess a = Web.Scotty.Trans.json $ JsonResponse "success" (fromList a)

jsonSimpleError :: RequestError -> ActionA a
jsonSimpleError a = do
    Web.Scotty.Trans.json $ JsonResponse "error" (fromList [("error", errorMessage a)])
    Web.Scotty.Trans.status $ statusCode a
    finish

checkAuthorized :: ActionT Error Config.ConfigM (Entity User)
checkAuthorized = do
    token <- maybe (jsonSimpleError unauthorizedUserError) return =<< getCookie "login_token"
    maybeUserEntity <- lift . validateRawToken $ Data.Text.unpack token
    maybe (jsonSimpleError unauthorizedUserError) return maybeUserEntity


handleMaybe :: a -> Maybe b -> Either a b
handleMaybe _ (Just a) = Right a
handleMaybe a Nothing  = Left a

application :: Config.Config -> ScottyT Error Config.ConfigM ()
application _ = do
    get "/api/:username/times" $ handleEither $ runExceptT $ do
        u <- lift $ param @String "username"
        user <- except . handleMaybe missingUserError =<< (lift . lift . getUserByUsername $ u)
        solveEntities <- lift . lift . getSolvesForUser $ user
        let solves = map (\(Entity _ (Solve m _)) -> m) solveEntities
        lift $ Web.Scotty.Trans.json solves
    post "/api/register" $ do
        UserForm u p <- jsonData @UserForm
        _ <- lift $ createUser u p
        jsonSimpleSuccess []
    post "/api/login" $ do
        UserForm u p <- jsonData @UserForm
        result <- runMaybeT $ do
            userEntity <- MaybeT . lift $ validateUserAndGet u p
            token <- lift . lift $ createLoginTokenAndGetToken userEntity
            return (Data.Text.Lazy.toStrict . Data.Text.Lazy.pack $ token)
        -- TODO make httpOnly, see below
        -- https://hackage.haskell.org/package/cookie-0.4.1.4/docs/Web-Cookie.html#t:SetCookie
        maybe (jsonSimpleError $ RequestError "Invalid login" status401) (setSimpleCookie "login_token") result
        jsonSimpleSuccess []
    post "/api/send-time" $ do
        userEntity <- checkAuthorized
        timeForm <- jsonData @TimeForm
        _ <- lift (addTimeForUser (millis timeForm) userEntity)
        jsonSimpleSuccess []
    get "/api/user/me" $ do
        token <- getCookie "login_token"
        case token of 
            Nothing -> jsonSimpleSuccess [("username", "Not logged in")]
            Just token -> do 
                maybeUserEntity <- lift . validateRawToken $ Data.Text.unpack token
                case maybeUserEntity of
                    Nothing -> jsonSimpleSuccess [("username", "Not logged in")]
                    Just (Entity _ u) -> jsonSimpleSuccess [("username", userUsername u)]
    notFound $ jsonSimpleError $ RequestError "HOLY FUCKING SHIT HOLY FUCK OH GOD OH NO" status404