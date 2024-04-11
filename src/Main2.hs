module Main (main) where

import qualified Data.HashMap.Lazy  as Map (toList)
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import           Data.Maybe         (fromMaybe)
import           Data.Word          (Word16)
import           Flag
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

data Test = Test
  { getTest0 :: String
  , getTest1 :: Int
  , getTest2 :: Bool
  } deriving Show

mkTest :: Test
mkTest = Test "" 0 False

flags :: NonEmpty (Flag Short)
flags =
    -- http flags
    define "http-host" "0.0.0.0" "http host" <|
    define "http-port" (9000 :: Word16) "http port" <|
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

setHttp :: Test -> (String,Value) -> Test
setHttp x ("http-host", v)  = x {getTest0 = test "" v}
setHttp x ("http-port", v)  = x {getTest1 = test 0 v}
setHttp x ("db-encrypt", v) = x {getTest2 = test False v}
setHttp x _                 = x

main :: IO ()
main = do
  args <- getArgs
  case parseArgs flags args of
    Left e -> do
      print e
      exitFailure
    Right v -> do
      print v
      let kv = Map.toList v
          res = foldl setHttp mkTest kv
      print res
