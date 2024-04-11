{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Flag
  ( Flag
  , Short
  , Long
  , Value

  , define
  , parseArgs
  , fromValue
  )
where

import           Internal.Process
import           Internal.Types     (Args (..), Error, Flag (..), Flags (..),
                                     Mode (..), NonEmptyArgs (..), Value (..),
                                     fromValue)

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, union)
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
           FlagName -> a -> Description -> Flag b
define n v = Flag n (Value v)
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
    Just v  ->
      parse @a (Flags flagsKV) (NEArgs v) >>= return . unArgs

parse :: Mode a =>
          Flags String Value -> NonEmptyArgs a -> Either Error (Args String Value)
parse flagsKV args = do
  argsKV <- process flagsKV args
  let fkv = unFlags flagsKV
      akv = (\case {Just x -> x; Nothing -> Value True}) <$> unArgs argsKV
  return $ Args $ Map.union akv fkv
