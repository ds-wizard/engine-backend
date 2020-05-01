module Shared.Util.Swagger where

import Control.Lens
import Data.Aeson
import Data.Swagger

import Shared.Util.JSON

simpleToSchema exampleDTO proxy =
  genericDeclareNamedSchema createSchemaOptions proxy & mapped . schema . example ?~ toJSON exampleDTO

createSchemaOptions :: SchemaOptions
createSchemaOptions = fromAesonOptions simpleOptions

simpleToSchema' fieldPrefix exampleDTO proxy =
  genericDeclareNamedSchema (createSchemaOptions' fieldPrefix) proxy & mapped . schema . example ?~ toJSON exampleDTO

createSchemaOptions' :: String -> SchemaOptions
createSchemaOptions' fieldPrefix = fromAesonOptions (createOptions fieldPrefix)

simpleToSchema'' fieldPrefix typeFieldName exampleDTO proxy =
  genericDeclareNamedSchema (createSchemaOptions'' fieldPrefix typeFieldName) proxy & mapped . schema . example ?~
  toJSON exampleDTO

createSchemaOptions'' :: String -> String -> SchemaOptions
createSchemaOptions'' fieldPrefix typeFieldName = fromAesonOptions (createOptions' fieldPrefix typeFieldName)
