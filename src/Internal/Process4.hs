module Internal.Process4 where

import           Internal.Types2

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, lookup, mapKeys)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE (fromList, nonEmpty, tail, take,
                                           toList)
import           Data.Maybe         (fromMaybe)
import           Data.Proxy
import           Data.Typeable      (Typeable, cast, typeOf, typeRep)
import           Text.Read          (readMaybe)

type FlagAppend = [(String, Maybe Value)] -> [(String, Maybe Value)]

data Strategy
  = SingleBool [(String, Maybe Value)]
  | KVPair FlagAppend
  | DistinctBool FlagAppend
  | DistinctArbitrary FlagAppend

data Short

instance Mode Short where
  process :: Flags String Value ->
             NonEmptyArgs Short ->
             Either Error (Args String (Maybe Value))
  process flagsKV args = do
    let enrichedFKV = Flags $ Map.mapKeys ('-' :) $ unFlags flagsKV
    argsKV <- checkAndBuild enrichedFKV (unNEArgs args)
    return . Args . Map.mapKeys (drop 1) $ Map.fromList argsKV
    where
      checkAndBuild :: Flags String Value -> NonEmpty String -> Either Error [(String, Maybe Value)]
      checkAndBuild (Flags m) xs = build $ NE.toList xs
        where
          build :: [String] -> Either Error [(String, Maybe Value)]
          build [] = Right []
          build xs = do
            case NE.nonEmpty (take 2 xs) of
              Nothing -> Right []
              Just v  -> do
                strategy <- deduceStrategy v
                case strategy of
                  SingleBool kv       -> return kv
                  KVPair f            -> f <$> build (drop 2 xs)
                  DistinctBool f      -> f <$> build (drop 2 xs)
                  DistinctArbitrary f -> f <$> build (drop 1 xs)

          deduceStrategy :: NonEmpty String -> Either Error Strategy
          deduceStrategy (x :| [])    = deduceStrategy' (x, Nothing)
          deduceStrategy (x :| (y:_)) = deduceStrategy' (x, Just y)

          deduceStrategy' :: (String, Maybe String) -> Either Error Strategy
          deduceStrategy' (x, Nothing) =
            case Map.lookup x m of
              Nothing -> Left $ UnknownFlag x
              Just v  -> doSingleCheck x v >> return (SingleBool [(x,Nothing)])
          deduceStrategy' (x, Just y) = go (Map.lookup x m, Map.lookup y m)
            where
              go :: (Maybe Value, Maybe Value) -> Either Error Strategy
              go (Nothing, _)      = Left $ UnknownFlag x
              go (Just v, Nothing) = doKVPairChecks (x,y) v >>= \pair -> return (KVPair (pair :))
              go (Just v, Just v') =
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
