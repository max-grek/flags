{-# LANGUAGE DerivingStrategies #-}

module Flag.Types5 where

import           Data.HashMap.Lazy  (HashMap)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Typeable

data ShortMode
data LongMode

data Flag a = Flag
  { getName        :: !String
  , getValue       :: !Value
  , getDescription :: !String
  } deriving Show

data Value = forall a . (Ord a, Read a, Show a, Typeable a) => Value a

instance Show Value where
  show (Value v) = show v

data Policy = Policy
  { modeLength :: Int
  , modePrefix :: String
  }

mkPolicy :: String -> Policy
mkPolicy []     = Policy 0 ""
mkPolicy prefix = Policy { modeLength = length prefix, modePrefix = prefix}

newtype Flags k v = Flags { unFlags :: HashMap k v} deriving newtype Show
newtype Args k v = Args { unArgs :: HashMap k v} deriving newtype Show
newtype NonEmptyArgs a = NEArgs { unNEArgs :: NonEmpty String } deriving newtype Show

class Mode a where
  enrichFlagName :: Flag a -> Flag a
  process :: Flags String Value -> NonEmptyArgs a -> Either Error (Args String String)

newtype UnknownFlagErr = UnknownFlag String deriving newtype Show
newtype FlagSyntaxErr = FlagSyntax String deriving newtype Show
newtype IncompatibleTypeErr = IncompatibleType String deriving newtype Show

data Error = UnknownFlag String
           | FlagSyntax String
           | IncompatibleType String

instance Show Error where
  show (UnknownFlag x)      = "unknown flag: " <> x
  show (FlagSyntax x)       = "flag syntax: " <> x
  show (IncompatibleType x) = "incompatible type: " <> show x
