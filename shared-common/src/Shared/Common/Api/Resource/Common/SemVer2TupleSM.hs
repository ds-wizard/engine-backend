module Shared.Common.Api.Resource.Common.SemVer2TupleSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Common.Model.Common.SemVer2Tuple

instance ToParamSchema SemVer2Tuple where
  toParamSchema _ =
    ParamSchema
      { _paramSchemaType = Just SwaggerString
      , _paramSchemaFormat = Nothing
      , _paramSchemaEnum = Nothing
      , _paramSchemaPattern = Just "^[0-9]+\\.[0-9]+$"
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

instance ToSchema SemVer2Tuple where
  declareNamedSchema _ = pure $ NamedSchema (Just "SemVer2Tuple") mempty
