{-# LANGUAGE DerivingStrategies #-}

module Flag.Types where

import           Data.HashMap.Lazy (HashMap)
import           Data.Typeable

data Short
data Long

data Flag a = Flag
  { getName        :: !String
  , getValue       :: !Value
  , getDescription :: !String
  } deriving Show

data Value = forall a . (Read a, Show a, Typeable a) => Value a

instance Show Value where
  show (Value v) = show v

newtype Flags k v = Flags { unFlags :: HashMap k v} deriving newtype Show
newtype Args k v = Args { unArgs :: HashMap k v} deriving newtype Show

data Error = UnknownFlag String
           | FlagSyntax String
           | IncompatibleType String

instance Show Error where
  show (UnknownFlag x)      = "unknown flag: " <> x
  show (FlagSyntax x)       = "flag syntax: " <> x
  show (IncompatibleType x) = "incompatible type: " <> show x
