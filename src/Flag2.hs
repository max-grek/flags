{-# LANGUAGE OverloadedStrings #-}

module Flag2
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
import           Data.Text          (Text)
import qualified Data.Text          as T (drop, intercalate, pack, splitOn,
                                          unpack)
import           Data.Typeable
import           Flag.Types2

type FlagName = String
type Description = String

define :: (Show a, Typeable a) => FlagName -> a -> Description -> NonEmpty (Flag b)
define n v d = Flag (T.pack n) (Value v) (T.pack d) :| []

parseArgs :: NonEmpty (Flag a) -> [String] -> Either Error (HashMap Text Value)
parseArgs flags args =
  let
    flagsKV = Map.fromList . NE.toList $ NE.zip (NE.map getName flags) (NE.map getValue flags)
  in case NE.nonEmpty args of
    Nothing -> Right flagsKV
    Just v  ->
      case _parse (Flags flagsKV) (NE.map T.pack v) of
        Left e  -> Left e
        Right v -> Right $ unArgs v

_parse :: Flags Text Value -> NonEmpty Text -> Either Error (Args Text Value)
_parse flagsKV args = do
  processed <- _process flagsKV args
  argsKV <- _compareTypes flagsKV processed
  let fkv = unFlags flagsKV
      akv = unArgs argsKV
  return . Args $ Map.intersection (Map.union akv fkv) fkv

_process :: Flags Text Value -> NonEmpty Text -> Either Error (Args Text Value)
_process flagsKV args =
  let argsKV = Map.fromList . mapMaybe (_splitBy "=" . T.drop 2) $ NE.toList args
  in do
    check_for_syntax args
    check_for_unknown (Map.keysSet $ unFlags flagsKV) (NE.fromList $ Map.keys argsKV)
    return $ Args argsKV
  where
    check_for_syntax :: NonEmpty Text -> Either Error ()
    check_for_syntax = go . map T.unpack . NE.toList
      where
        go :: [String] -> Either Error ()
        go [] = Right ()
        go (x:xs)
          | take 2 x == "--" = go xs
          | otherwise = Left $ FlagSyntax x

    check_for_unknown :: HashSet Text -> NonEmpty Text -> Either Error ()
    check_for_unknown set = go . map T.unpack . NE.toList
      where
        go :: [String] -> Either Error ()
        go [] = Right ()
        go (x:xs)
          | Set.member (T.pack x) set = go xs
          | otherwise = Left $ UnknownFlag x

_compareTypes :: Flags Text Value -> Args Text Value -> Either Error (Args Text Value)
_compareTypes flags args = go (Map.keys $ unFlags flags) flags args
  where
    go :: [Text] -> Flags Text Value -> Args Text Value -> Either Error (Args Text Value)
    go [] _ args = Right args
    go (x:xs) flags args =
      let ftype = _lookupType x $ unFlags flags
          atype = _lookupType x $ unArgs args
      in case compare' ftype atype of
           Just e  -> Left . IncompatibleType . T.unpack $ msg x e
           Nothing -> go xs flags args

    compare' :: Maybe TypeRep -> Maybe TypeRep -> Maybe (TypeRep, TypeRep)
    compare' Nothing _         = Nothing
    compare' _ Nothing         = Nothing
    compare' (Just x) (Just y) = if x == y then Nothing else Just (x,y)

    msg :: Text -> (TypeRep, TypeRep) -> Text
    msg x (t1,t2) =
      let flagName = "defined flag " <> x
          defined = " has type " <> show t1
          passed = ", but passed type is " <> show t2
      in flagName <> T.pack defined <> T.pack passed

_lookupType :: Text -> HashMap Text Value -> Maybe TypeRep
_lookupType k m = Map.lookup k m >>= \(Value v) -> Just $ typeOf v

_splitBy :: Text -> Text -> Maybe (Text,Value)
_splitBy delim xs =
  case T.splitOn delim xs of
    (x:ys) -> Just (x, Value $ T.intercalate delim ys)
    _      -> Nothing
