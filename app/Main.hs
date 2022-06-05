{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid (h1_, p_)
import Web.Spock (SpockM, get, root, runSpock, spock)
import Web.Spock.Config (
    PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
 )
import Web.Spock.Lucid (lucid)

-- https://haskell-at-work.com/episodes/2018-04-09-your-first-web-application-with-spock.html
--
-- Run with:
--  ghcid -T :main

type Server a = SpockM () () () a

app :: Server ()
app = get root $
    lucid $ do
        h1_ "Lucid templates..."
        p_ "Are awesome!!"

main :: IO ()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock cfg app)
