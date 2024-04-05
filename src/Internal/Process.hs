module Internal.Process
  ( Mode
  , Short
  , Long

  , process
  ) where

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, lookup, mapKeys)
import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (fromList, toList)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (mapMaybe)
import           Data.Typeable      (typeOf)
import           Internal.Types
import           Text.Read          (readMaybe)

data Strategy = KVPair | Distinct | DistinctTyped

data Short
data Long

class Mode a where
  process :: Flags String Value -> NonEmptyArgs a -> Either Error (Args String Value)

instance Mode Short where
  process :: Flags String Value -> NonEmptyArgs Short -> Either Error (Args String Value)
  process flagsKV args = do
    let enrichedFKV = Flags $ Map.mapKeys ('-' :) $ unFlags flagsKV
    argsKV <- checkAndBuild enrichedFKV (unNEArgs args)
    return . Args . Map.mapKeys (drop 1) $ Map.fromList $ (fmap . fmap) Value argsKV
    where
      checkAndBuild :: Flags String Value -> NonEmpty String -> Either Error [(String,String)]
      checkAndBuild (Flags m) xs = go m (NE.toList xs)
        where
          go :: HashMap String Value -> [String] -> Either Error [(String,String)]
          go _ [] = Right []
          -- todo
          go m [x] = _forUnknown x m >>= _forBoolArg x >> Right [(x,"")]
          go m xss@(x:y:xs) = do
            strategy <- deduceStrategy (x,y) m
            case strategy of
              KVPair        -> ((x,y) :) <$> go m xs
              Distinct      -> ((x,"") :) . ((y,"") :) <$> go m xs
              DistinctTyped -> ((x,"") :) <$> go m (tail xss)

deduceStrategy :: (String,String) -> HashMap String Value -> Either Error Strategy
deduceStrategy pair@(x,y) m = go pair (Map.lookup x m, Map.lookup y m)
  where
    go :: (String,String) -> (Maybe Value,Maybe Value) -> Either Error Strategy
    go pair (Nothing, _) = Left . UnknownFlag $ fst pair
    go pair (Just v, Nothing) = check pair v >> return KVPair
    go pair (Just (Value v), Just (Value v')) =
      case compare ("Bool" == show (typeOf v)) ("Bool" == show (typeOf v')) of
        EQ -> Right Distinct
        LT -> Left . FlagSyntax $ fst pair <> " must have arg"
        GT -> Right DistinctTyped

check :: (String,String) -> Value -> Either Error ()
check pair v = checkForArg (fst pair) v >> checkForCompatibility pair  v

checkForArg :: String -> Value -> Either Error ()
checkForArg x (Value v)
  | "Bool" == show (typeOf v) = Left . FlagSyntax $ x <> " must not have arg"
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
  | "Bool" == show (typeOf v) = Right ()
  | otherwise = Left . FlagSyntax $ x <> " must have arg"

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
          go' m [] = Right ()
          go' m (x:xs) = _forUnknown (fst x) m >>= _forBoolArg (fst x) >> go' m xs

_splitBy :: String -> String -> Maybe (String,String)
_splitBy delim xs =
  case splitOn delim xs of
    (x:ys) -> Just (x, intercalate delim ys)
    _      -> Nothing