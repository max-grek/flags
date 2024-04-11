module Internal.Process.Short
  ( Short

  , process
  ) where

import           Internal.Types

import qualified Data.HashMap.Lazy  as Map (fromList, lookup, mapKeys)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (nonEmpty, toList)
import           Data.Proxy
import           Data.Typeable      (typeOf, typeRep)
import           Text.Read          (readMaybe)

type KV = [(String, Maybe Value)]
type KVAppend = KV -> KV

data Strategy
  = SingleBool KV
  | KVPair KVAppend
  | DistinctBool KVAppend
  | DistinctArbitrary KVAppend

data Short

instance Mode Short where
  process :: Flags String Value ->
             NonEmptyArgs Short ->
             Either Error (Args String (Maybe Value))
  process flagsKV args = do
    let enrichedFKV = Flags $ Map.mapKeys ('-' :) $ unFlags flagsKV
    argsKV <- build enrichedFKV (unNEArgs args)
    return . Args . Map.mapKeys (drop 1) $ Map.fromList argsKV
    where
      build :: Flags String Value -> NonEmpty String -> Either Error [(String, Maybe Value)]
      build (Flags m) xs = go $ NE.toList xs
        where
          go :: [String] -> Either Error [(String, Maybe Value)]
          go [] = Right []
          go xs = case NE.nonEmpty (take 2 xs) of
            Nothing -> Right []
            Just candidates -> do
              strategy <- deduceStrategy candidates
              case strategy of
                SingleBool kv       -> return kv
                KVPair f            -> f <$> go (drop 2 xs)
                DistinctBool f      -> f <$> go (drop 2 xs)
                DistinctArbitrary f -> f <$> go (drop 1 xs)

          deduceStrategy :: NonEmpty String -> Either Error Strategy
          deduceStrategy (x :| [])    = deduceStrategy' (x, Nothing)
          deduceStrategy (x :| (y:_)) = deduceStrategy' (x, Just y)

          deduceStrategy' :: (String, Maybe String) -> Either Error Strategy
          deduceStrategy' (x, Nothing) =
            case Map.lookup x m of
              Nothing -> Left $ UnknownFlag x
              Just v  -> doSingleCheck x v >> return (SingleBool [(x,Nothing)])
          deduceStrategy' (x, Just y) = deduce (Map.lookup x m, Map.lookup y m)
            where
              deduce :: (Maybe Value, Maybe Value) -> Either Error Strategy
              deduce (Nothing, _)      = Left $ UnknownFlag x
              deduce (Just v, Nothing) = doKVPairChecks (x,y) v >>= \pair -> return (KVPair (pair :))
              deduce (Just v, Just v') =
                case compare (valueIsBool v) (valueIsBool v') of
                  EQ -> Right . DistinctBool $ ((x,Nothing) :) . ((y,Nothing) :)
                  LT -> Left . FlagSyntax $ x <> " must have arg"
                  GT -> Right $ DistinctArbitrary ((x,Nothing) :)

doSingleCheck :: String -> Value -> Either Error ()
doSingleCheck x v
  | not $ valueIsBool v = Left . FlagSyntax $ x <> " must have arg"
  | otherwise = Right ()

doKVPairChecks :: (String,String) -> Value -> Either Error (String, Maybe Value)
doKVPairChecks (x,y) val@(Value v)
  | valueIsBool val = Left . FlagSyntax $ x <> " must not have argument"
  | typeOf v == typeRep (Proxy @String) = Right (x, Just $ Value y)
  | otherwise =
    case transform y of
      Nothing -> Left . IncompatibleType $ x <> " must have " <> show (typeOf v) <> " type"
      Just vv -> Right (x, Just vv)
  where
    transform :: String -> Maybe Value
    transform x = readMaybe' v x >>= Just . Value
      where
        readMaybe' :: forall a . Read a => a -> String -> Maybe a
        readMaybe' _ = readMaybe @a

valueIsBool :: Value -> Bool
valueIsBool (Value v) = typeOf v == typeRep (Proxy @Bool)
