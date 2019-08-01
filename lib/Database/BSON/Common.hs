module Database.BSON.Common where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Map (Map, fromList, toList)
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

instance ToBSON (Map String String) where
  toBSON m = fmap (\(k, v) -> (T.pack k) BSON.=: v) (toList m)

instance FromBSON (Map String String) where
  fromBSON doc = Just . fromList . fmap (\f -> (T.unpack . BSON.label $ f, BSON.typed . BSON.value $ f)) $ doc
