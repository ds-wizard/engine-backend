module Shared.Util.Aeson where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.String (fromString)
import qualified Data.Text as T
import GHC.Generics

import Shared.Util.Reflection (HasConstructor, constructorName)
import Shared.Util.String (stripSuffixIfExists)

jsonOptions :: Options
jsonOptions = defaultOptions {fieldLabelModifier = jsonSpecialFields}

jsonOptionsWithTypeField :: String -> Options
jsonOptionsWithTypeField typeFieldName =
  defaultOptions
    { fieldLabelModifier = jsonSpecialFields
    , tagSingleConstructors = True
    , sumEncoding = TaggedObject {tagFieldName = typeFieldName, contentsFieldName = "contents"}
    , constructorTagModifier = stripSuffixIfExists "DTO" . stripSuffixIfExists "'"
    }

-- ---------------------------------------------------------------------------------------------------------------------
toJSONWithAdditionalData :: (Generic a, GToJSON' Value Zero (Rep a)) => [(Key, Value)] -> a -> Value
toJSONWithAdditionalData additionalData dto =
  case genericToJSON jsonOptions dto of
    Object o -> Object $ KM.union o (KM.fromList additionalData)

toSumJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
toSumJSON = genericToJSON (defaultOptions {sumEncoding = UntaggedValue})

toSumJSONWithTypeField :: (Generic a, GToJSON Zero (Rep a), HasConstructor (Rep a)) => String -> String -> a -> Value
toSumJSONWithTypeField typeFieldName suffix dto =
  case genericToJSON (defaultOptions {sumEncoding = UntaggedValue}) dto of
    Object o -> do
      let typeMap = KM.fromList [(fromString typeFieldName, String . T.pack . stripSuffixIfExists suffix . stripSuffixIfExists "'" . constructorName $ dto)]
      Object $ KM.union typeMap o

-- ---------------------------------------------------------------------------------------------------------------------
jsonSpecialFields :: String -> String
jsonSpecialFields "bundleId" = "id"
jsonSpecialFields "intId" = "id"
jsonSpecialFields "aId" = "id"
jsonSpecialFields "dId" = "id"
jsonSpecialFields "gId" = "id"
jsonSpecialFields "iId" = "id"
jsonSpecialFields "lId" = "id"
jsonSpecialFields "pId" = "id"
jsonSpecialFields "qiId" = "id"
jsonSpecialFields "sId" = "id"
jsonSpecialFields "tId" = "id"
jsonSpecialFields "aData" = "data"
jsonSpecialFields "seData" = "data"
jsonSpecialFields "scData" = "data"
jsonSpecialFields "sqData" = "data"
jsonSpecialFields "ouiData" = "data"
jsonSpecialFields "qeData" = "data"
jsonSpecialFields "aType" = "type"
jsonSpecialFields "gType" = "type"
jsonSpecialFields "pType" = "type"
jsonSpecialFields "aLabel" = "label"
jsonSpecialFields "oRole" = "role"
jsonSpecialFields "uRole" = "role"
jsonSpecialFields "sValue" = "value"
jsonSpecialFields "aValue" = "value"
jsonSpecialFields "mcValue" = "value"
jsonSpecialFields "ilValue" = "value"
jsonSpecialFields "iValue" = "value"
jsonSpecialFields field = field
