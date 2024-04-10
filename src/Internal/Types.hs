{-# LANGUAGE DerivingStrategies #-}

module Internal.Types
  ( Flag (..)
  , Flags (..)
  , Args (..)
  , NonEmptyArgs (..)
  , Value (..)
  , Error (..)

  , fromValue
  )
where

import           Control.Monad      (guard)
import           Data.HashMap.Lazy  (HashMap)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Proxy
import           Data.Typeable      (Typeable, cast, typeOf, typeRep)

data Flag a = Flag
  { getName        :: !String
  , getValue       :: !Value
  , getDescription :: !String
  }

data Value = forall a . (Ord a, Read a, Show a, Typeable a) => Value a

fromValue :: forall a . Typeable a => Value -> Maybe a
fromValue (Value v)
  | typeOf v == typeRep (Proxy @a) = cast v
  | otherwise = Nothing

instance Show Value where
  show (Value v) = show v

newtype Flags k v = Flags
  { unFlags :: HashMap k v
  } deriving newtype Show

newtype Args k v = Args
  { unArgs :: HashMap k v
  } deriving newtype Show

newtype NonEmptyArgs a = NEArgs
  { unNEArgs :: NonEmpty String
  } deriving newtype Show

data Error
  = UnknownFlag String
  | FlagSyntax String
  | IncompatibleType String

instance Show Error where
  show (UnknownFlag x)      = "unknown flag: " <> x
  show (FlagSyntax x)       = "flag syntax: " <> x
  show (IncompatibleType x) = "incompatible type: " <> show x
