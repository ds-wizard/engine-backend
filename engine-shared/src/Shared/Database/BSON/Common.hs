module Shared.Database.BSON.Common where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.UUID as U

instance BSON.Val U.UUID where
  val = BSON.String . U.toText
  cast' (BSON.String x) = U.fromText x
  cast' (BSON.Sym (BSON.Symbol x)) = U.fromText x
  cast' _ = Nothing

instance ToBSON (String, String) where
  toBSON (key, value) = ["key" BSON.=: key, "value" BSON.=: value]

instance FromBSON (String, String) where
  fromBSON doc = do
    key <- BSON.lookup "key" doc
    value <- BSON.lookup "value" doc
    return (key, value)

instance ToBSON (M.Map String String) where
  toBSON m = fmap (\(k, v) -> T.pack k BSON.=: v) (M.toList m)

instance FromBSON (M.Map String String) where
  fromBSON = Just . M.fromList . fmap (\f -> (T.unpack . BSON.label $ f, BSON.typed . BSON.value $ f))

instance ToBSON a => ToBSON (M.Map U.UUID a) where
  toBSON m = fmap (\(k, v) -> U.toText k BSON.=: toBSON v) (M.toList m)

instance FromBSON a => FromBSON (M.Map U.UUID a) where
  fromBSON =
    Just .
    M.fromList .
    fmap (\f -> (fromJust . U.fromText . BSON.label $ f, fromJust . fromBSON . BSON.typed . BSON.value $ f))

instance ToBSON (M.Map String [U.UUID]) where
  toBSON m = fmap (\(k, v) -> T.pack k BSON.=: v) (M.toList m)

instance FromBSON (M.Map String [U.UUID]) where
  fromBSON = Just . M.fromList . fmap (\f -> (T.unpack . BSON.label $ f, BSON.typed . BSON.value $ f))

genericStringMapToBSON :: ToBSON a => M.Map String a -> BSON.Document
genericStringMapToBSON m = fmap (\(k, v) -> T.pack k BSON.=: toBSON v) (M.toList m)

defaultStringMapFromBSON :: FromBSON a => BSON.Document -> Maybe (M.Map String a)
defaultStringMapFromBSON =
  Just . M.fromList . fmap (\f -> (T.unpack . BSON.label $ f, fromJust . fromBSON . BSON.typed . BSON.value $ f))
