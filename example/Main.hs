module Main (main) where

import           Config
import qualified Data.HashMap.Lazy  as Map (toList)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Word          (Word16)
import           Flag
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

data Config = Config Http deriving stock Show

data Http
  = Http
  { getHttpHost  :: !String
  , getHttpPort  :: !Word16
  , getHttpBool1 :: !Bool
  , getHttpBool2 :: !Bool
  }
  deriving stock (Show)

flags :: NonEmpty (Flag Short)
flags =
    -- http flags
    define "http-host" "0.0.0.0" "http host"  <>
    define "http-port" (9000 :: Int) "http port" <>
    define "http-bool" Flase "http bool exampl2" <>
    define "http-bool2" False "http bool2 example"

-- | If you've ever worked with existential types
-- | how you deal with getting the term back?
setHttp :: Http -> (String,Value) -> Http
setHttp x ("http-host", Value v)  = x {getHttpHost = show v} -- | here I have to use show to get data from Value, but when I set it to Http structure, value of that type then looks like this: "\"lol\""
-- | since I apply show to the string
setHttp x ("http-port", Value v)  = x {getHttpPort = read @Word16 $ show v} -- | the same is here, I have to use show and then work read
setHttp x ("http-bool1", Value v) = x {getHttpBool1 = read @Bool $ show v} -- | and so on
setHttp x ("http-bool2", Value v) = x {getHttpBool2 = read @Bool $ show v} -- | and so forth
setHttp x _                       = x
-- | do you know any idea how to prevent this or is this only one possible way to get the data from existential type?

main :: IO ()
main = do
  args <- getArgs
  case parseArgs flags args of
    Left e -> do
      print e
      exitFailure
    Right v -> do
      let kv = Map.toList v
          result = foldl setHttp mkHttp kv
          cfg = Config result
      print cfg
