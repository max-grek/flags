{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.List.NonEmpty (NonEmpty (..), (<|))
import           Data.Word          (Word16)
import           Flag
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

flags :: NonEmpty (Flag Long)
flags =
    -- http flags
    define "http-host" "0.0.0.0" "http host" <|
    define "http-port" (9000 :: Int) "http port" <|
    define "http-timeout" "15s" "http timeout" <|
    -- storage flags
    define "db-driver" "postgres" "database driver" <|
    define "db-user" "amc_dev" "database user" <|
    define "db-password" "amcdev" "database password" <|
    define "db-name" "amcdb_dev" "database name" <|
    define "db-schema" "userdata" "database schema" <|
    define "db-mode" "disable" "database mode" <|
    define "dsn" "postgresql://amc_dev:amcdev@0.0.0.0:5433/amcdb_dev?sslmode=disable" "database dsn" <|
    define "db-host" "0.0.0.0" "database host" <|
    define "db-port" (5433 :: Int) "database port" <|
    define "db-enable-dsn" False "switch between dsn and ..." <|
    define "db-encrypt" False "database data encryption" <|
    -- redis flags
    define "redis-host" "0.0.0.0" "redis host" <|
    define "redis-port" (0 :: Int) "redis port" :| []

main :: IO ()
main = do
  args <- getArgs
  print $ parseArgs flags args