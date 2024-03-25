module Main (main) where

--import           Flag               hiding (flags)
import           Data.List.NonEmpty
import           Flag3              hiding (flags)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

flags :: NonEmpty (Flag Short)
flags =
   -- http descriptor
    define "http-host" "0.0.0.0" "http host"  <>
    define "http-port" (9000 :: Int) "http port" <>
    define "http-timeout" "15s" "http timeout" <>
    -- storage descriptor
    define "db-driver" "postgres" "database driver" <>
    define "db-user" "amc_dev" "database user" <>
    define "db-password" "amcdev" "database password" <>
    define "db-name" "amcdb_dev" "database name" <>
    define "db-schema" "userdata" "database schema" <>
    define "db-mode" "disable" "database mode" <>
    define "dsn" "postgresql://amc_dev:amcdev@0.0.0.0:5433/amcdb_dev?sslmode=disable" "database dsn" <>
    define "db-host" "0.0.0.0" "database host" <>
    define "db-port" (5433 :: Int) "database port" <>
    define "db-enable-dsn" False "switch between dsn and ..." <>
    define "db-encrypt" False "database data encryption" <>
    -- redis descriptor
    define "redis-host" "0.0.0.0" "redis host" <>
    define "redis-port" (0 :: Int) "redis port"

main :: IO ()
main = do
  args <- getArgs
  case parseArgs flags args of
    Left e -> do
      print e
      exitFailure
    Right v -> print v
