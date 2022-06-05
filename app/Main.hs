{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config

-- https://haskell-at-work.com/episodes/2018-04-09-your-first-web-application-with-spock.html
--
-- Run with:
--  ghcid -T :main

type Server a = SpockM () () () a

app :: Server ()
app = get root (html "<h1>Hello!!</h1>")

main :: IO ()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock cfg app)
