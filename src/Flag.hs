{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Flag
  ( Flag
  , Short
  , Long

  , define
  , parseArgs
  )
where

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, intersection, mapKeys,
                                            union)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (nonEmpty, toList)
import           Data.Typeable      (Typeable)
import           Internal.Process   (Long, Mode, Short, process)
import           Internal.Types     (Args (..), Error, Flag (..), Flags (..),
                                     NonEmptyArgs (..), Value (..))

type FlagName = String
type Description = String

define :: (Ord a, Read a, Show a, Typeable a) => FlagName -> a -> Description -> NonEmpty (Flag b)
define n v d = Flag n (Value v) d :| []

parseArgs :: forall a . Mode a => NonEmpty (Flag a) -> [String] -> Either Error (HashMap String Value)
parseArgs flags args = do
  let flags' = NE.toList flags
      flagsKV = Map.fromList $ zip (map getName flags') (map getValue flags')
  case NE.nonEmpty args of
    Nothing -> return flagsKV
    Just v -> do
      argsKV <- _parse @a (Flags flagsKV) (NEArgs v)
      return $ unArgs argsKV

_parse :: Mode a => Flags String Value -> NonEmptyArgs a -> Either Error (Args String Value)
_parse flagsKV args = do
  argsKV <- process flagsKV args
  let fkv = unFlags flagsKV
      akv = unArgs argsKV
  return . Args $ Map.intersection (Map.union akv fkv) fkv
