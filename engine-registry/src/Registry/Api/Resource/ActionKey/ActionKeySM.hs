module Registry.Api.Resource.ActionKey.ActionKeySM where

import Data.Swagger

import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Api.Resource.ActionKey.ActionKeyJM ()
import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Registry.Model.ActionKey.ActionKey
import Shared.Util.Swagger

instance ToSchema ActionKeyType

instance ToSchema ActionKeyDTO where
  declareNamedSchema = simpleToSchema forgTokActionKeyDto
