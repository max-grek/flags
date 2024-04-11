module Internal.Process.Long
  ( Long

  , process
  ) where

import           Internal.Types     (Args (..), Error (..), Flags (..),
                                     Mode (..), NonEmptyArgs (..), Value (..))

import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as Map (fromList, lookup, mapKeys)
import qualified Data.List.NonEmpty as NE (fromList, toList)
import           Data.Proxy         (Proxy (..))
import           Data.Typeable      (typeOf, typeRep)
import           Text.Read          (readMaybe)

data Long

instance Mode Long where
  process :: Flags String Value ->
             NonEmptyArgs Long ->
             Either Error (Args String (Maybe Value))
  process (Flags flagsKV) (NEArgs args) =
    let argsKV = map (`splitBy` '=') $ NE.toList args
        enrichedFKV = Map.mapKeys (('-' :) . ('-' :)) flagsKV
    in do
      argsKV <- build enrichedFKV argsKV
      return . Args . Map.mapKeys (drop 2) $ Map.fromList argsKV
    where
      build :: HashMap String Value -> [(String, Maybe String)] -> Either Error [(String, Maybe Value)]
      build _ [] = Right []
      build m (pair@(k,_):xs) = go pair (Map.lookup k m) >>= \x -> (x :) <$> build m xs
        where
          go :: (String, Maybe String) -> Maybe Value -> Either Error (String, Maybe Value)
          go (x,_) Nothing  = Left $ UnknownFlag x
          go pair@(x,y) (Just v) =
            case checkForArg pair v of
              Just e -> Left e
              Nothing ->
                case transform pair v of
                  Just v -> Right v
                  Nothing -> Left . IncompatibleType $ x <> " must have " <> show (typeOf y) <> " type"

transform :: (String, Maybe String) -> Value -> Maybe (String, Maybe Value)
transform (x, Nothing) _ = Just (x, Just $ Value True)
transform (x, Just y) (Value v)
  | typeOf v == typeRep (Proxy @String) = Just (x, Just $ Value y)
  | otherwise = readMaybe' v y >>= \val -> Just (x, Just $ Value val)
  where
    readMaybe' :: forall a . Read a => a -> String -> Maybe a
    readMaybe' _ = readMaybe @a

checkForArg :: (String, Maybe String) -> Value -> Maybe Error
checkForArg (x, Nothing) value
  | not $ valueIsBool value = Just . FlagSyntax $ x <> " must have argument"
checkForArg (x, Just _) value
  | valueIsBool value = Just . FlagSyntax $ x <> " must not have argument"
checkForArg _ _ = Nothing

valueIsBool :: Value -> Bool
valueIsBool (Value v) = typeOf v == typeRep (Proxy @Bool)

splitBy :: String -> Char -> (String, Maybe String)
splitBy xs delim =
  case break (== delim) xs of
    (x, [])  -> (x, Nothing)
    (x,rest) -> (x, Just $ drop 1 rest)
