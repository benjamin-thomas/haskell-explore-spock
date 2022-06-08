{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (
    forM_,
    void,
 )
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.String as Text
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Lucid (ToHtml (toHtml), br_, data_, form_, h1_, h2_, h3_, head_, href_, input_, label_, li_, link_, method_, name_, rel_, script_, src_, textarea_, title_, type_, ul_, value_)
import qualified Lucid.Base

import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import Network.Wai (Middleware)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)

import Data.Int (Int32)

import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Typed (PGConnection, pgExecute, pgQuery)
import Database.PostgreSQL.Typed.Query (pgSQL)
import Network.WebSockets (Connection, ServerApp, acceptRequest, defaultConnectionOptions, sendTextData)
import System.Environment (getEnv)
import Web.Spock (HasSpock (getState), SpockM, get, middleware, param', post, redirect, root, runSpock, spock)
import Web.Spock.Config (
    PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
 )
import Web.Spock.Lucid (lucid)

{-
https://haskell-at-work.com/episodes/2018-04-09-your-first-web-application-with-spock.html

Run with:
  PORT=4000 ghcid -T :main

Display with (prerequisite):
  apt install html-xml-utils

Display with (prerequisite):
  curl -s localhost:8080 | hxnormalize

Live reload (NOTE: httpie handles retries faster):
  find . | entr -c http localhost:8080
  find . | entr -c bash -c "curl -s --retry 5 --retry-connrefused localhost:8080|hxnormalize|head"
-}

type Server a = SpockM () () ServerState a

data Note = Note
    { author :: Text
    , contents :: Text
    }

newtype ServerState = ServerState {state :: IORef (UTCTime, [Note])}

toEpoch :: UTCTime -> String
toEpoch = formatTime defaultTimeLocale "%s"

toEpochHtml :: Monad m => UTCTime -> Lucid.Base.HtmlT m ()
toEpochHtml t = toHtml (toEpoch t)

toEpochText :: UTCTime -> Text
toEpochText t = Text.fromString (toEpoch t)

-- https://github.com/chrisdone/lucid/issues/30
scriptTag :: Monad m => Text -> Lucid.Base.HtmlT m ()
scriptTag s = script_ [src_ s] emptyContent
  where
    emptyContent :: Text
    emptyContent = ""

-- WEBSOCKET / AUTO-REFRESH

counter :: Connection -> Int -> IO ()
counter conn i = do
    threadDelay 500000 -- 500ms
    sendTextData conn (T.pack $ show i)
    counter conn (i + 1)

wsApp :: ServerApp
wsApp pendingConn = do
    conn <- acceptRequest pendingConn
    counter conn 1

wsMiddleware :: Middleware
wsMiddleware = websocketsOr defaultConnectionOptions wsApp

-- SERVER

app :: Server ()
app = do
    middleware (staticPolicy (addBase "static"))
    middleware logStdoutDev
    middleware wsMiddleware
    get root $ do
        (t, notes') <- getState >>= (liftIO . readIORef . state)
        lucid $ do
            head_ [Lucid.data_ "srv-start" (toEpochText t)] $ do
                title_ "Speck demo!"
                link_ [href_ "/css/main.css", rel_ "stylesheet"]
                scriptTag "/js/reload.js"

            h1_ "Notes"
            h2_ "\"live\" reload on port 4000"
            h3_ (toEpochHtml t)

            ul_ $
                forM_ notes' $ \note -> li_ $ do
                    toHtml (author note)
                    ": "
                    toHtml (contents note)
            h2_ "New note"
            form_ [method_ "post"] $ do
                label_ $ do
                    "Author: "
                    input_ [name_ "author"]
                br_ []
                label_ $ do
                    "Contents:"
                    textarea_ [name_ "contents"] ""
                input_ [type_ "submit", value_ "Add note"]

    post root $ do
        newAuthor <- param' "author"
        newContents <- param' "contents"
        ref <- state <$> getState
        liftIO $
            atomicModifyIORef' ref $ \(start_, notes_) ->
                ((start_, notes_ <> [Note newAuthor newContents]), ())
        redirect "/"

main :: IO ()
main = do
    now <- getCurrentTime
    state_ <-
        ServerState
            <$> newIORef
                ( now
                ,
                    [ Note "Bob" "Learn some Haskell"
                    , Note "John" "Keep learning Haskell"
                    ]
                )

    cfg <- defaultSpockCfg () PCNoDatabase state_

    port <- read <$> getEnv "PORT"
    runSpock port (spock cfg app)

-- DATABASE

data Client = Client Int32 String
    deriving (Eq)

data Client2 = Client2 {id_ :: Int32, name :: String}
    deriving (Show)

clientFromRow :: (Int32, String) -> Client2
clientFromRow (a, b) = newClient a b

newClient :: Int32 -> String -> Client2
newClient id2 name2 = Client2{id_ = id2, name = name2}

insertClient :: PGConnection -> Client -> IO ()
insertClient pg (Client cid name) =
    void $
        pgExecute
            pg
            [pgSQL|INSERT INTO clients (id, name) VALUES (${cid}, ${name})|]

getClient :: PGConnection -> Int32 -> IO (Maybe Client)
getClient pg cid =
    fmap (uncurry Client) . listToMaybe
        <$> pgQuery
            pg
            [pgSQL|SELECT id, name FROM clients WHERE id = ${cid}|]

listClients :: PGConnection -> IO [Client2]
listClients pg = do
    rows <-
        pgQuery
            pg
            [pgSQL|SELECT id, name FROM clients LIMIT 10|]
    pure
        (fmap clientFromRow rows)