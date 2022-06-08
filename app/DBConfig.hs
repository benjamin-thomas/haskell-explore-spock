{-# LANGUAGE OverloadedStrings #-}

module DBConfig where

import Data.ByteString.UTF8 as BSU
import Database.PostgreSQL.Typed
import System.Environment

utf8 :: String -> ByteString
utf8 = BSU.fromString

dbConn :: IO PGDatabase
dbConn = do
    pgHost <- getEnv "PGHOST"
    pgPort <- getEnv "PGPORT"
    pgUser <- getEnv "PGUSER"
    pgPassword <- getEnv "PGPASSWORD"
    pgDatabase <- getEnv "PGDATABASE"

    putStrLn $ "--> Connecting to host: " ++ pgHost

    return
        defaultPGDatabase
            { pgDBAddr = Left (pgHost, pgPort)
            , pgDBUser = utf8 pgUser
            , pgDBPass = utf8 pgPassword
            , pgDBName = utf8 pgDatabase
            }
