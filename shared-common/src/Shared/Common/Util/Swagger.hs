module Shared.Common.Util.Swagger where

import Data.Aeson
import Data.Swagger

import Shared.Common.Util.Aeson

toSwagger exampleDTO proxy =
  let schema = genericDeclareNamedSchema (fromAesonOptions jsonOptions) proxy
   in fmap (\s -> s {_namedSchemaSchema = s._namedSchemaSchema {_schemaExample = Just . toJSON $ exampleDTO}}) schema

toSwaggerWithType typeFieldName exampleDTO proxy =
  let schema = genericDeclareNamedSchema (fromAesonOptions (jsonOptionsWithTypeField typeFieldName)) proxy
   in fmap (\s -> s {_namedSchemaSchema = s._namedSchemaSchema {_schemaExample = Just . toJSON $ exampleDTO}}) schema

toSwaggerWithFlatType typeFieldName exampleDTO proxy =
  let schema = genericDeclareNamedSchemaUnrestricted (fromAesonOptions (jsonOptionsWithTypeField typeFieldName)) proxy
   in fmap (\s -> s {_namedSchemaSchema = s._namedSchemaSchema {_schemaExample = Just . toJSON $ exampleDTO}}) schema

toSwaggerWithDtoName dtoName exampleDTO proxy =
  let schema = genericDeclareNamedSchema ((fromAesonOptions jsonOptions) {fieldLabelModifier = changePageFields}) proxy
   in fmap (\s -> s {_namedSchemaName = Just dtoName, _namedSchemaSchema = s._namedSchemaSchema {_schemaExample = Just . toJSON $ exampleDTO}}) schema

changePageFields :: String -> String
changePageFields "name" = "name"
changePageFields "metadata" = "page"
changePageFields "entities" = "_embedded"
changePageFields field = field
