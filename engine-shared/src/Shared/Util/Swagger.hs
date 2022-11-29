module Shared.Util.Swagger where

import Control.Lens
import Data.Aeson
import Data.Swagger

import Shared.Util.Aeson

toSwagger exampleDTO proxy =
  genericDeclareNamedSchema (fromAesonOptions jsonOptions) proxy & mapped . schema . example ?~ toJSON exampleDTO

toSwaggerWithType typeFieldName exampleDTO proxy =
  genericDeclareNamedSchema (fromAesonOptions (jsonOptionsWithTypeField typeFieldName)) proxy
    & mapped . schema . example
      ?~ toJSON exampleDTO

toSwaggerWithDtoName dtoName exampleDTO proxy =
  genericDeclareNamedSchema ((fromAesonOptions jsonOptions) {fieldLabelModifier = changePageFields}) proxy
    & mapped
      . schema
      . example
      ?~ toJSON exampleDTO
    & mapped
      . name
      ?~ dtoName

changePageFields :: String -> String
changePageFields "name" = "name"
changePageFields "metadata" = "page"
changePageFields "entities" = "_embedded"
changePageFields field = field
