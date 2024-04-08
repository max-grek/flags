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

import           Internal.Process   (Long, Mode, Short, process)
import           Internal.Types     (Args (..), Error, Flag (..), Flags (..),
                                     NonEmptyArgs (..), Value (..))

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, intersection, union)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (nonEmpty, toList)
import           Data.Typeable      (Typeable)

type FlagName = String
type Description = String

{- |
   'define' allows set up specific flag.
   It takes flag's name, flag's value and flag's description and returns NonEmpty list.
   Pay attention, that flag's value is part of existential quantification.
   Also Flag type uses phantom type, so when user wants to declare top level definition,
   he/she must choose between 2 options - Short flag or Long flag. That's crucial thing
   for processing and making distinction between different flag syntaxes.
   Pay attention, that flag name must be without dashes.
-}
define :: (Ord a, Read a, Show a, Typeable a) =>
          FlagName -> a -> Description -> NonEmpty (Flag b)
define n v d = Flag n (Value v) d :| []

{- |
   'parseArgs' parses incoming arguments from command line.
   It takes flags that must be defined via 'define' function and
   list of arguments. It produces Error or key value parsed arguments.
-}
parseArgs :: forall a . Mode a =>
             NonEmpty (Flag a) -> [String] -> Either Error (HashMap String Value)
parseArgs flags args = do
  let flags' = NE.toList flags
      flagsKV = Map.fromList $ zip (map getName flags') (map getValue flags')
  case NE.nonEmpty args of
    Nothing -> return flagsKV
    Just v  -> do
      argsKV <- _parse @a (Flags flagsKV) (NEArgs v)
      return $ unArgs argsKV

{- |
   '_parse' processes incoming arguments with defined flags
-}
_parse :: Mode a =>
          Flags String Value -> NonEmptyArgs a -> Either Error (Args String Value)
_parse flagsKV args = do
  argsKV <- process flagsKV args
  let fkv = unFlags flagsKV
      akv = unArgs argsKV
  return . Args $ Map.intersection (Map.union akv fkv) fkv
