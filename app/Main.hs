{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.String as Text
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format
import Lucid (ToHtml (toHtml), br_, data_, form_, h1_, h2_, head_, href_, input_, label_, li_, link_, method_, name_, rel_, script_, src_, textarea_, title_, type_, ul_, value_)
import qualified Lucid.Base
import Network.Wai.Middleware.Static
import Web.Spock (HasSpock (getState), SpockM, get, middleware, param', post, redirect, root, runSpock, spock)
import Web.Spock.Config (
    PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
 )
import Web.Spock.Lucid (lucid)

{-
https://haskell-at-work.com/episodes/2018-04-09-your-first-web-application-with-spock.html

Run with:
  ghcid -T :main

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

app :: Server ()
app = do
    middleware (staticPolicy (addBase "static"))
    get root $ do
        (t, notes') <- getState >>= (liftIO . readIORef . state)
        lucid $ do
            head_ [Lucid.data_ "srv-start" (toEpochText t)] $ do
                title_ "Speck demo!"
                link_ [href_ "/css/main.css", rel_ "stylesheet"]
                scriptTag "/js/reload.js"
            h1_ "Notes"

            h2_ (toEpochHtml t)

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
    runSpock 8080 (spock cfg app)
