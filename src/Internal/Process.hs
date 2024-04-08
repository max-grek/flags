module Internal.Process
  ( Mode
  , Short
  , Long

  , process
  ) where

import           Internal.Types     (Args (..), Error (..), Flags (..),
                                     NonEmptyArgs (..), Value (..))

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, lookup, mapKeys)
import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (fromList, toList)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (mapMaybe)
import           Data.Typeable      (typeOf)
import           Text.Read          (readMaybe)

data Strategy
  = KVPair
  | DistinctBool
  | DistinctArbitrary

-- | is that good idea to leave parts of phantom type Flag here
-- | or you have another idea or approach of how to do that more correctly?
data Short
data Long

class Mode a where
  process :: Flags String Value -> NonEmptyArgs a -> Either Error (Args String Value)

instance Mode Short where
  process :: Flags String Value -> NonEmptyArgs Short -> Either Error (Args String Value)
  process flagsKV args = do
    let enrichedFKV = Flags $ Map.mapKeys ('-' :) $ unFlags flagsKV -- ^ enrich flags' keys with one dash
    argsKV <- checkAndBuild enrichedFKV (unNEArgs args)
    return . Args . Map.mapKeys (drop 1) $ Map.fromList $ (fmap . fmap) Value argsKV
    where
      checkAndBuild :: Flags String Value -> NonEmpty String -> Either Error [(String,String)]
      checkAndBuild (Flags m) xs = go m (NE.toList xs)
        where
          go :: HashMap String Value -> [String] -> Either Error [(String,String)]
          go _ [] = Right []
          -- | check for unknown flag and is bool flag has arg or not
          go m [x] = _forUnknown x m >>= _forBoolArg x >> Right [(x,"")]
          go m xss@(x:y:xs) = do
            strategy <- _deduceStrategy (x,y) m -- | here I deduce strategy based on 2 arguments
            case strategy of
              KVPair            -> ((x,y) :) <$> go m xs -- | -http-port 20 -> (http-port,20) :
              DistinctBool      -> ((x,"") :) . ((y,"") :) <$> go m xs -- | -boolean1 -boolean2 -> (-boolean1,""),(-boolean2,"") :
              DistinctArbitrary -> ((x,"") :) <$> go m (tail xss) -- | -boolean -http-port -> make (boolean,"") and go to the next recursion with the second flag

_deduceStrategy :: (String,String) -> HashMap String Value -> Either Error Strategy
_deduceStrategy pair@(x,y) m = go pair (Map.lookup x m, Map.lookup y m)
  where
    go :: (String,String) -> (Maybe Value,Maybe Value) -> Either Error Strategy
    go pair (Nothing, _) = Left . UnknownFlag $ fst pair -- | if there is no flag in map - error
    go pair (Just v, Nothing) = _applyChecks pair v >> return KVPair -- | here we have flag and arg -> do checks
    go pair (Just (Value v), Just (Value v')) = -- | here we have 2 flags
      case compare ("Bool" == show (typeOf v)) ("Bool" == show (typeOf v')) of -- | now we need to deduce what kind of flags these are
        EQ -> Right DistinctBool -- | if they equal, than they are distinct bool flags
        LT -> Left . FlagSyntax $ fst pair <> " must have arg" -- | if first arg is non-boolean flag and second flag is a bool flag -> error, non-bool flag must have arg
        GT -> Right DistinctArbitrary -- | Left arg is bool flag, second arg is non-bool flag

_applyChecks :: (String,String) -> Value -> Either Error ()
_applyChecks pair v = do
  check_for_arg (fst pair) v -- | if bool's flag, it musnt' have arg (error), otherwise - ok
  check_for_compatibility pair  v -- | if flag's type is not compatible what user typed - error
  where
    check_for_arg :: String -> Value -> Either Error ()
    check_for_arg x (Value v)
      | "Bool" == show (typeOf v) = Left . FlagSyntax $ x <> " must not have arg"
      | otherwise = Right ()

    check_for_compatibility :: (String,String) -> Value -> Either Error ()
    check_for_compatibility pair (Value v)
      | "[Char]" == show (typeOf v) = Right ()
      | otherwise =
        case readMaybe' v (snd pair) of
          Nothing -> Left . IncompatibleType $ fst pair <> " must have " <> show (typeOf v) <> " type"
          _       -> Right ()
      where
        readMaybe' :: forall a . Read a => a -> String -> Maybe a
        readMaybe' _ = readMaybe @a

instance Mode Long where
  process :: Flags String Value -> NonEmptyArgs Long -> Either Error (Args String Value)
  process flagsKV args =
    let enrichedFKV = Flags $ Map.mapKeys ("--" <>) $ unFlags flagsKV
        argsKV = mapMaybe (_splitBy "=") $ NE.toList (unNEArgs args)
    in do
      go enrichedFKV (NE.fromList argsKV)
      return . Args . Map.mapKeys (drop 2) $ Map.fromList $ (fmap . fmap) Value argsKV
    where
      go :: Flags String Value -> NonEmpty (String,String) -> Either Error ()
      go (Flags m) xs = go' m $ NE.toList xs
        where
          go' :: HashMap String Value -> [(String,String)] -> Either Error ()
          go' m []     = Right ()
          go' m (x:xs) = test x m >> go' m xs

-- | don't know how to name it
-- logic is the same as was in Short flags
test :: (String,String) -> HashMap String Value -> Either Error ()
test pair@(k,_) m = go pair $ Map.lookup k m
  where
    go :: (String,String) -> Maybe Value -> Either Error ()
    go pair Nothing  = Left . UnknownFlag $ fst pair
    go pair (Just v) = check_for_arg pair v >> checkForCompatibility pair v

    check_for_arg :: (String,String) -> Value -> Either Error ()
    check_for_arg pair (Value v)
      | "Bool" == show (typeOf v) && (not . null $ snd pair) =
        Left . FlagSyntax $ fst pair <> " must not have arg"
      | "Bool" /= show (typeOf v) && null (snd pair) =
        Left . FlagSyntax $ fst pair <> " must have arg"
      | otherwise = Right ()

checkForCompatibility :: (String,String) -> Value -> Either Error ()
checkForCompatibility pair (Value v)
  | "[Char]" == show (typeOf v) = Right ()
  | otherwise =
    case readMaybe' v (snd pair) of
      Nothing -> Left . IncompatibleType $ fst pair <> " must have " <> show (typeOf v) <> " type"
      _       -> Right ()
  where
    readMaybe' :: forall a . Read a => a -> String -> Maybe a
    readMaybe' _ = readMaybe @a

_forUnknown :: String -> HashMap String Value -> Either Error Value
_forUnknown k m = go $ Map.lookup k m
  where
    go :: Maybe Value -> Either Error Value
    go Nothing  = Left $ UnknownFlag k
    go (Just v) = Right v

_forBoolArg :: String -> Value -> Either Error ()
_forBoolArg x (Value v)
  | "Bool" == show (typeOf v) = Left . FlagSyntax $ x <> " must not have arg"
  | otherwise = Right ()

_splitBy :: String -> String -> Maybe (String,String)
_splitBy delim xs =
  case splitOn delim xs of
    (x:ys) -> Just (x, intercalate delim ys)
    _      -> Nothing
