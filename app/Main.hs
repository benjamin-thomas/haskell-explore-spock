{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Lucid (ToHtml (toHtml), h1_, li_, ul_)
import Web.Spock (HasSpock (getState), SpockM, get, param', post, redirect, root, runSpock, spock)
import Web.Spock.Config (
    PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
 )
import Web.Spock.Lucid (lucid)

-- import Data.Semigroup ((<>))

-- https://haskell-at-work.com/episodes/2018-04-09-your-first-web-application-with-spock.html
--
-- Run with:
--  ghcid -T :main

type Server a = SpockM () () ServerState a

data Note = Note
    { author :: Text
    , contents :: Text
    }

newtype ServerState = ServerState {notes :: IORef [Note]}

app :: Server ()
app = do
    get root $ do
        notes' <- getState >>= (liftIO . readIORef . notes)
        lucid $ do
            h1_ "Notes"
            ul_ $
                forM_ notes' $ \note -> li_ $ do
                    toHtml (author note)
                    ": "
                    toHtml (contents note)
    post root $ do
        newAuthor <- param' "author"
        newContents <- param' "contents"
        notesRef <- notes <$> getState
        liftIO $
            atomicModifyIORef' notesRef $ \notes_ ->
                (notes_ <> [Note newAuthor newContents], ())
        redirect "/"

main :: IO ()
main = do
    state <-
        ServerState
            <$> newIORef
                [ Note "Bob" "Learn some Haskell"
                , Note "John" "Keep learning Haskell"
                ]

    cfg <- defaultSpockCfg () PCNoDatabase state
    runSpock 8080 (spock cfg app)
