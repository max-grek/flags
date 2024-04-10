module Internal.Process2
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
import           Data.Proxy         (Proxy (..))
import           Data.Typeable      (typeOf, typeRep)
import           Text.Read          (readMaybe)

data Strategy
  = KVPair
  | DistinctBool
  | DistinctArbitrary

data Short
data Long

class Mode a where
  process :: Flags String Value -> NonEmptyArgs a -> Either Error (Args String (Maybe Value))

instance Mode Short where
  process :: Flags String Value -> NonEmptyArgs Short -> Either Error (Args String (Maybe Value))
  process flagsKV args = do
    let enrichedFKV = Flags $ Map.mapKeys ('-' :) $ unFlags flagsKV
    argsKV <- checkAndBuild enrichedFKV (unNEArgs args)
    return . Args . Map.mapKeys (drop 1) $ Map.fromList argsKV
    where
      checkAndBuild :: Flags String Value -> NonEmpty String -> Either Error [(String,Maybe Value)]
      checkAndBuild (Flags m) xs = go m (NE.toList xs)
        where
          go :: HashMap String Value -> [String] -> Either Error [(String,Maybe Value)]
          go _ []           = Right []
          go m [x]          = deduceStrategy2 (x,"") m >>= \v -> v <$> go m []
          go m xss@(x:y:xs) = deduceStrategy2 (x,y) m >>= \v -> v <$> go m xs

          deduceStrategy2 :: (String,String) -> HashMap String Value -> Either Error ([(String, Maybe Value)] -> [(String, Maybe Value)])
          deduceStrategy2 pair@(x,y) m
            | null y = go (Map.lookup x m, Nothing)
            | otherwise = go (Map.lookup x m, Map.lookup y m)
            where
              go :: (Maybe Value,Maybe Value) -> Either Error ([(String, Maybe Value)] -> [(String, Maybe Value)])
              go (Nothing, _) = Left $ UnknownFlag x
              go (Just v, Nothing)
                | null y =z Right ((x,Nothing) :)
                | valueIsBool v = Left . FlagSyntax $ x <> " must not have arg"
                | otherwise = checkForCompatibility pair v >> return ((x,Just $ Value y) :)
              go (Just v, Just v') =
                case compare (valueIsBool v) (valueIsBool v') of
                  EQ -> Right $ ((x,Nothing) :) . ((y,Nothing) :)
                  LT -> Left . FlagSyntax $ x <> " must have arg"
                  GT -> Right ((x,Nothing) :)

          deduceStrategy :: (String,String) -> HashMap String Value -> Either Error Strategy
          deduceStrategy pair@(x,y) m
            | null y = go (Map.lookup x m, Nothing)
            | otherwise = go (Map.lookup x m, Map.lookup y m)
            where
              go :: (Maybe Value,Maybe Value) -> Either Error Strategy
              go (Nothing, _) = Left $ UnknownFlag x
              go (Just v, Nothing)
                | valueIsBool v = Left . FlagSyntax $ x <> " must not have arg"
                | otherwise = checkForCompatibility pair v >> return KVPair
              go (Just v, Just v') =
                case compare (valueIsBool v) (valueIsBool v') of
                  EQ -> Right DistinctBool
                  LT -> Left . FlagSyntax $ x <> " must have arg"
                  GT -> Right DistinctArbitrary

test :: (String,String) -> HashMap String Value -> Either Error ()
test pair@(k,v) m =
  case Map.lookup k m of
    Nothing  -> Left $ UnknownFlag k
    Just val -> checkForArg val >> checkForCompatibility pair val
  where
    checkForArg :: Value -> Either Error ()
    checkForArg value
      | valueIsBool value && not (null v) =
        Left . FlagSyntax $ k <> " must not have arg"
      | not (valueIsBool value) && null v =
        Left . FlagSyntax $ k <> " must have arg"
      | otherwise = Right ()

checkForCompatibility :: (String,String) -> Value -> Either Error ()
checkForCompatibility pair val@(Value v)
  | valueIsString val = Right ()
  | otherwise =
    case readMaybe' v (snd pair) of
      Nothing -> Left . IncompatibleType $ fst pair <> " must have " <> show (typeOf v) <> " type"
      _       -> Right ()
  where
    readMaybe' :: forall a . Read a => a -> String -> Maybe a
    readMaybe' _ = readMaybe @a

valueIsBool :: Value -> Bool
valueIsBool (Value v) = typeOf v == typeRep (Proxy @Bool)

valueIsString :: Value -> Bool
valueIsString (Value v) = typeOf v == typeRep (Proxy @String)

_splitBy :: String -> String -> Maybe (String,String)
_splitBy delim xs =
  case splitOn delim xs of
    (x:ys) -> Just (x, intercalate delim ys)
    _      -> Nothing

-- instance Mode Long where
--   process :: Flags String Value -> NonEmptyArgs Long -> Either Error (Args String (Maybe Value))
--   process flagsKV args =
--     let enrichedFKV = Flags $ Map.mapKeys ("--" <>) $ unFlags flagsKV
--         argsKV = mapMaybe (_splitBy "=") $ NE.toList (unNEArgs args)
--     in do
--       go enrichedFKV (NE.fromList argsKV)
--       return . Args . Map.mapKeys (drop 2) $ Map.fromList argsKV
--     where
--       go :: Flags String Value -> NonEmpty (String,String) -> Either Error ()
--       go (Flags m) xs = go' m $ NE.toList xs
--         where
--           go' :: HashMap String Value -> [(String,String)] -> Either Error ()
--           go' m []     = Right ()
--           go' m (x:xs) = test x m >> go' m xs
