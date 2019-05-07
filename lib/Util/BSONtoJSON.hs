module Util.BSONtoJSON
  ( mapBSONDocumentToJSONObject
  , mapBSONValueToJSONValue
  ) where

import Data.Aeson.Types as JSON
import Data.Bson as BSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

mapBSONDocumentToJSONObject :: BSON.Document -> JSON.Object
mapBSONDocumentToJSONObject = HashMap.fromList . map (\(key := value) -> (key, mapBSONValueToJSONValue value))

mapBSONValueToJSONValue :: BSON.Value -> JSON.Value
mapBSONValueToJSONValue (BSON.Doc doc) = JSON.Object $ mapBSONDocumentToJSONObject doc
mapBSONValueToJSONValue (BSON.Array list) = JSON.Array . Vector.fromList . map mapBSONValueToJSONValue $ list
mapBSONValueToJSONValue (BSON.ObjId oId) = toJSON . show $ oId
mapBSONValueToJSONValue (BSON.String string) = toJSON string
mapBSONValueToJSONValue (BSON.Int32 number) = toJSON number
mapBSONValueToJSONValue (BSON.Int64 number) = toJSON number
mapBSONValueToJSONValue (BSON.Float number) = toJSON number
mapBSONValueToJSONValue (BSON.Bool bool) = toJSON bool
mapBSONValueToJSONValue (BSON.UTC time) = toJSON time
mapBSONValueToJSONValue (BSON.Stamp (BSON.MongoStamp stamp)) = toJSON stamp
mapBSONValueToJSONValue BSON.Null = JSON.Null
