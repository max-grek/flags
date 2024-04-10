module Internal.Process3
  ( Mode
  , Short
  , Long

  , process
  ) where

import           Internal.Types     (Args (..), Error (..), Flags (..),
                                     NonEmptyArgs (..), Value (..))

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, lookup, mapKeys)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (fromList, toList)
import           Data.Maybe         (fromMaybe)
import           Data.Proxy         (Proxy (..))
import           Data.Typeable      (typeOf, typeRep)
import           Text.Read          (readMaybe)

data Strategy
  = KVPair -- | (-flag, 1)
  | DistinctBool -- | (-boolFlag1, -boolFlag2)
  | DistinctArbitrary -- | (-boolFlag1, -flag)

data Short

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
      checkAndBuild (Flags m) xs = go $ NE.toList xs
        where
          go :: [String] -> Either Error [(String,Maybe Value)]
          go []           = Right []
          go [x]          =
            case Map.lookup x m of
              Nothing -> Left $ UnknownFlag x
              Just v -> if not $ valueIsBool v
                        then Left . FlagSyntax $ x <> " must have arg"
                        else Right [(x,Nothing)]
          go xss@(x:y:xs) = do
            strategy <- deduceStrategy (x,y)
            case strategy of
              KVPair            -> ((x,Just $ Value y) :) <$> go xs
              DistinctBool      -> ((x,Nothing) :) . ((y,Nothing) :) <$> go xs
              DistinctArbitrary -> ((x,Nothing) :) <$> go (tail xss)

          deduceStrategy :: (String,String) -> Either Error Strategy
          deduceStrategy pair@(x,y) = go (Map.lookup x m, Map.lookup y m)
            where
              go :: (Maybe Value, Maybe Value) -> Either Error Strategy
              go (Nothing, _)      = Left $ UnknownFlag x
              go (Just v, Nothing)
                | valueIsBool v = Left $ FlagSyntax $ x <> " must not have arg"
                | otherwise = checkForCompatibility pair v >> return KVPair
              go (Just v, Just v') =
                case compare (valueIsBool v) (valueIsBool v') of
                  EQ -> Right DistinctBool
                  LT -> Left . FlagSyntax $ x <> " must have arg"
                  GT -> Right DistinctArbitrary

data Long

instance Mode Long where
  process :: Flags String Value -> NonEmptyArgs Long -> Either Error (Args String (Maybe Value))
  process (Flags flagsKV) (NEArgs args) =
    let argsKV = map (`splitBy` '=') $ NE.toList args
        enrichedFKV = Map.mapKeys (('-' :) . ('-' :)) flagsKV
    in do
      argsKV <-go enrichedFKV argsKV
      return . Args . Map.mapKeys (drop 2) $ Map.fromList argsKV
    where
      go :: HashMap String Value -> [(String, Maybe String)] -> Either Error [(String, Maybe Value)]
      go _ [] = Right []
      go m (pair@(k,_):xs) =
        case Map.lookup k m of
          Nothing -> Left $ UnknownFlag k
          Just v -> do
            checkForArg pair v
            transform pair v >>= \x -> (x :) <$> go m xs

      transform :: (String, Maybe String) -> Value -> Either Error (String, Maybe Value)
      transform (x, Nothing) _  = Right (x, Just $ Value True)
      transform (x, Just y) (Value v)
        | typeOf v == typeRep (Proxy @String) = Right (x, Just $ Value y)
        | otherwise =
          case readMaybe' v y of
            Nothing -> Left . IncompatibleType $ x <> " must have " <> show (typeOf y) <> "type"
            Just v -> Right (x, Just $ Value v)
        where
          readMaybe' :: forall a . Read a => a -> String -> Maybe a
          readMaybe' _ = readMaybe @a

      checkForArg :: (String, Maybe String) -> Value -> Either Error ()
      checkForArg (x, Nothing) value
        | not $ valueIsBool value = Left . FlagSyntax $ x <> " must have arg"
      checkForArg (x, Just _) value
        | valueIsBool value = Left . FlagSyntax $ x <> " must not have arg"
      checkForArg _ _ = Right ()

lol :: String -> Value -> Maybe Value
lol x (Value v)
  | typeOf v == typeRep (Proxy @String) = Just $ Value x
  | otherwise = readMaybe' v x >>= Just . Value
  where
    readMaybe' :: forall a . Read a => a -> String -> Maybe a
    readMaybe' _ = readMaybe @a

checkForCompatibility2 :: (String, String) -> Value -> Either Error Value
checkForCompatibility2 (x,y) (Value v)
  | typeOf v == typeRep (Proxy @String) = Right $ Value y
  | otherwise =
    case readMaybe' v y of
      Nothing -> Left . IncompatibleType $ x <> " must have " <> show (typeOf v) <> " type"
      Just val       -> Right $ Value val
  where
    readMaybe' :: forall a . Read a => a -> String -> Maybe a
    readMaybe' _ = readMaybe @a

checkForCompatibility :: (String, String) -> Value -> Either Error ()
checkForCompatibility (x,y) (Value v)
  | typeOf v == typeRep (Proxy @String) = Right ()
  | otherwise =
    case readMaybe' v y of
      Nothing -> Left . IncompatibleType $ x <> " must have " <> show (typeOf v) <> " type"
      _       -> Right ()
  where
    readMaybe' :: forall a . Read a => a -> String -> Maybe a
    readMaybe' _ = readMaybe @a

valueIsBool :: Value -> Bool
valueIsBool (Value v) = typeOf v == typeRep (Proxy @Bool)

splitBy :: String -> Char -> (String, Maybe String)
splitBy xs delim =
  case break (== delim) xs of
    (x, [])  -> (x, Nothing)
    (x,rest) -> (x, Just $ drop 1 rest)
