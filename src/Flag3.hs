{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Flag3
  ( Flag
  , Short
  --, Long

  , define
  , parseArgs
  ) where

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, intersection, keys,
                                            keysSet, lookup, member, toList,
                                            union)
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as Set (member)
import           Data.List          (intercalate, isPrefixOf)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (fromList, map, nonEmpty, toList, zip)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (mapMaybe)
import           Data.Typeable      (Typeable, typeOf)
import           Flag.Types3
import           Text.Read          (readMaybe)

type FlagName = String
type Description = String

define :: (Read a, Show a, Typeable a) => FlagName -> a -> Description -> NonEmpty (Flag b)
define n v d = Flag n (Value v) d :| []

parseArgs :: forall a . Mode a => NonEmpty (Flag a) -> [String] -> Either Error (HashMap String Value)
parseArgs flags args =
  let flagsKV = Map.fromList . NE.toList $ NE.zip (NE.map getName flags) (NE.map getValue flags)
  in case NE.nonEmpty args of
    Nothing -> return flagsKV
    Just neArgs  ->
      case _parse @a (Flags flagsKV) (NEArgs neArgs) of
        Left e  -> Left e
        Right v -> Right $ unArgs v

_parse :: Mode a => Flags String Value -> NonEmptyArgs a -> Either Error (Args String Value)
_parse flagsKV args = do
  processedArgsKV <- process flagsKV args
  argsKV <- _compareTypes flagsKV processedArgsKV
  let fkv = unFlags flagsKV
      akv = unArgs argsKV
  return . Args $ Map.intersection (Map.union akv fkv) fkv

-- newtype Long = L LongMode

-- instance Mode Long where
--   _process flagsKV args =
--     let args' = unNEArgs args
--         argsKV = Map.fromList . mapMaybe (_splitBy "=" . drop 2) $ NE.toList args'
--         flagsKeys = Map.keysSet $ unFlags flagsKV
--         argsKeys = NE.fromList $ Map.keys argsKV
--     in do
--       _checkForSyntax (mkPolicy "--") args'
--       _checkForUnknown flagsKeys argsKeys
--       return $ Args argsKV
--     where
--       process' :: Flags String Value -> NonEmpty String -> Either Error [(String,String)]
--       process' flagsKV args = undefined
--         where
--       _checkForSyntax :: Policy -> NonEmpty String -> Either Error ()
--       _checkForSyntax p = go . NE.toList
--         where
--           go :: [String] -> Either Error ()
--           go [] = Right ()
--           go (x:xs)
--             | take (modeLength p) x == modePrefix p = go xs
--             | otherwise = Left $ FlagSyntax x

--       _checkForUnknown :: HashSet String -> NonEmpty String -> Either Error ()
--       _checkForUnknown set = go . NE.toList
--         where
--           go :: [String] -> Either Error ()
--           go [] = Right ()
--           go (x:xs)
--             | Set.member x set = go xs
--             | otherwise = Left $ UnknownFlag x

newtype Short = S ShortMode

_checkForSyntax :: NonEmpty String -> Either Error ()
_checkForSyntax = go . NE.toList
  where
    go :: [String] -> Either Error ()
    go [] = Right ()
    go (x:xs)
      | take 1 x == "-" = go xs
      | otherwise = undefined

instance Mode Short where
  parse2 :: Flags String Value -> NonEmptyArgs Short -> Either Error (Args String String)
  parse2 flagsKV args =
    let rmDashes = NE.map (\x -> if isPrefixOf "-" x then drop 1 x else x) (unNEArgs args)
    in case process flagsKV (NEArgs rmDashes) of
         Left e  -> Left e
         Right v -> return . Args $ Map.fromList v

  process :: Flags String Value -> NonEmptyArgs Short -> Either Error [(String,String)]
  process m xs = go (NE.toList $ unNEArgs xs) (unFlags m)
    where
      go :: [String] -> HashMap String Value -> Either Error [(String,String)]
      go [] m = Right []
      go [x] m
        | Map.member x m = Right [(x,"")]
        | otherwise = Left $ UnknownFlag x
      go [x,y] m =
        case rot (Map.member x m) (Map.member y m) of
          Just (Left f)  -> Left $ f x
          Just (Right _) -> Right [(x,y)]
          Nothing        -> Right [(x,""),(y,"")]
      go xss@(x:y:xs) m =
        case rot (Map.member x m) (Map.member y m) of
          Just (Left f)  -> Left $ f x
          Just (Right _) -> ((x,y) :) <$> go xs m
          Nothing        -> ((x,"") :) <$> go (tail xss) m

      rot :: Bool -> Bool -> Maybe (Either (String -> Error) Bool)
      rot False True = Just . Left $ UnknownFlag
      rot True False = Just $ Right False
      rot _ _        = Nothing

newtype LeftMember = LM Bool
newtype RightMember = RM Bool

eitherBool :: LeftMember -> RightMember -> Maybe (Either (String -> Error) RightMember)
eitherBool (LM False) (RM True) = Just . Left $ UnknownFlag
eitherBool (LM True) (RM False) = Just . Right $ RM False
eitherBool _ _                  = Nothing

_splitBy :: String -> String -> Maybe (String,String)
_splitBy delim xs =
  case splitOn delim xs of
    (x:ys) -> Just (x, intercalate delim ys)
    _      -> Nothing

_compareTypes :: Flags String Value -> Args String String -> Either Error (Args String Value)
_compareTypes flagsKV argsKV = go (Map.toList $ unArgs argsKV) (unFlags flagsKV)
  where
    go :: [(String,String)] -> HashMap String Value -> Either Error (Args String Value)
    go argsKV flagsKV =
      case go' argsKV flagsKV of
        Left e  -> Left e
        Right v -> return . Args . Map.fromList $ (fmap . fmap) Value v
      where
        go' :: [(String,String)] -> HashMap String Value -> Either Error [(String,String)]
        go' [] flagsKV = return argsKV
        go' (x:xs) flagsKV =
          case Map.lookup (fst x) flagsKV of
            Nothing        -> go' xs flagsKV
            Just (Value v) ->
              if "[Char]" == show (typeOf v)
              then go' xs flagsKV
              else
                case readMaybe' v (snd x) of
                  Nothing -> Left . IncompatibleType $ fst x <> " must have " <> show (typeOf v) <> " type"
                  _ -> go' xs flagsKV

    readMaybe' :: forall a . Read a => a -> String -> Maybe a
    readMaybe' v = readMaybe @a

-- flags :: NonEmpty (Flag Long)
-- flags =
--    -- http descriptor
--     define "http-host" "0.0.0.0" "http host"  <>
--     define "http-port" (9000 :: Int) "http port" <>
--     define "http-timeout" "15s" "http timeout" <>
--     -- storage descriptor
--     define "db-driver" "postgres" "database driver" <>
--     define "db-user" "amc_dev" "database user" <>
--     define "db-password" "amcdev" "database password" <>
--     define "db-name" "amcdb_dev" "database name" <>
--     define "db-schema" "userdata" "database schema" <>
--     define "db-mode" "disable" "database mode" <>
--     define "dsn" "postgresql://amc_dev:amcdev@0.0.0.0:5433/amcdb_dev?sslmode=disable" "database dsn" <>
--     define "db-host" "0.0.0.0" "database host" <>
--     define "db-port" (5433 :: Int) "database port" <>
--     define "db-enable-dsn" False "switch between dsn and ..." <>
--     define "db-encrypt" False "database data encryption" <>
--     -- redis descriptor
--     define "redis-host" "0.0.0.0" "redis host" <>
--     define "redis-port" (0 :: Int) "redis port"
