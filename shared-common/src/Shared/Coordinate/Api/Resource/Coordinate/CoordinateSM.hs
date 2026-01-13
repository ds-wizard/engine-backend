module Shared.Coordinate.Api.Resource.Coordinate.CoordinateSM where

import Data.Swagger

import Shared.Coordinate.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Coordinate.Model.Coordinate.Coordinate

instance ToParamSchema Coordinate where
  toParamSchema _ =
    ParamSchema
      { _paramSchemaType = Just SwaggerString
      , _paramSchemaFormat = Nothing
      , _paramSchemaEnum = Nothing
      , _paramSchemaPattern = Just "^[0-9]+\\.[0-9]+\\.[0-9]+$"
      , _paramSchemaMaximum = Nothing
      , _paramSchemaExclusiveMaximum = Nothing
      , _paramSchemaMinimum = Nothing
      , _paramSchemaExclusiveMinimum = Nothing
      , _paramSchemaMaxLength = Nothing
      , _paramSchemaMinLength = Nothing
      , _paramSchemaItems = Nothing
      , _paramSchemaDefault = Nothing
      , _paramSchemaMaxItems = Nothing
      , _paramSchemaMinItems = Nothing
      , _paramSchemaUniqueItems = Nothing
      , _paramSchemaMultipleOf = Nothing
      }

instance ToSchema Coordinate where
  declareNamedSchema _ = pure $ NamedSchema (Just "Coordinate") mempty
