{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Flag
  ( Flag
  , Short
  , Long

  , define
  , parseArgs
  ) where

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, intersection, keys,
                                            keysSet, lookup, union)
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as Set (member)
import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (fromList, map, nonEmpty, toList, zip)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (mapMaybe)
import           Data.Typeable      (Typeable, typeOf)
import           Flag.Types
import           Text.Read          (readEither, readMaybe)

type FlagName = String
type Description = String

define :: (Read a, Show a, Typeable a) => FlagName -> a -> Description -> NonEmpty (Flag b)
define n v d = Flag n (Value v) d :| []

parseArgs :: NonEmpty (Flag a) -> [String] -> Either Error (HashMap String Value)
parseArgs flags args =
  let
    flagsKV = Map.fromList . NE.toList $ NE.zip (NE.map getName flags) (NE.map getValue flags)
  in case NE.nonEmpty args of
    Nothing -> Right flagsKV
    Just v  ->
      case _parse (Flags flagsKV) v of
        Left e  -> Left e
        Right v -> Right $ unArgs v

_parse :: Flags String Value -> NonEmpty String -> Either Error (Args String Value)
_parse flagsKV args = do
  argsKV <- _process flagsKV args
  let fkv = unFlags flagsKV
      akv = unArgs argsKV
  return . Args $ Map.intersection (Map.union akv fkv) fkv

_process :: Flags String Value -> NonEmpty String -> Either Error (Args String Value)
_process flagsKV args =
  let argsKV = Map.fromList . mapMaybe (_splitBy "=" . drop 2) $ NE.toList args
  in do
    check_for_syntax args
    check_for_unknown (Map.keysSet $ unFlags flagsKV) (NE.fromList $ Map.keys argsKV)
    _compareTypes flagsKV args
  where
    check_for_syntax :: NonEmpty String -> Either Error ()
    check_for_syntax = go . NE.toList
      where
        go :: [String] -> Either Error ()
        go [] = Right ()
        go (x:xs)
          | take 2 x == "--" = go xs
          | otherwise = Left $ FlagSyntax x

    check_for_unknown :: HashSet String -> NonEmpty String -> Either Error ()
    check_for_unknown set = go . NE.toList
      where
        go [] = Right ()
        go (x:xs)
          | Set.member x set = go xs
          | otherwise = Left $ UnknownFlag x

_compareTypes :: Flags String Value -> NonEmpty String -> Either Error (Args String Value)
_compareTypes flagsKV args =
  let argsKV = mapMaybe (_splitBy "=" . drop 2) $ NE.toList args
  in go argsKV (unFlags flagsKV)
  where
    go :: [(String,String)] -> HashMap String Value -> Either Error (Args String Value)
    go argsKV flagsKV =
      case go' argsKV flagsKV of
        Left e  -> Left e
        Right v -> return . Args . Map.fromList $ (fmap . fmap) Value v
      where
        go' :: [(String,String)] -> HashMap String Value -> Either Error [(String,String)]
        go' [] flagsKV = Right argsKV
        go' (x:xs) flagsKV =
          case Map.lookup (fst x) flagsKV of
            Nothing -> go' xs flagsKV
            Just (Value v) ->
              if "[Char]" == show (typeOf v)
              then go' xs flagsKV
              else
                case readMaybe' v (snd x) of
                  Nothing -> Left . IncompatibleType $ fst x <> " must have " <> show (typeOf v) <> " type"
                  _       -> go' xs flagsKV

    readMaybe' :: forall a . Read a => a -> String -> Maybe a
    readMaybe' v = readMaybe @a

_splitBy :: String -> String -> Maybe (String,String)
_splitBy delim xs =
  case splitOn delim xs of
    (x:ys) -> Just (x, intercalate delim ys)
    _      -> Nothing

flags :: NonEmpty (Flag Long)
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


