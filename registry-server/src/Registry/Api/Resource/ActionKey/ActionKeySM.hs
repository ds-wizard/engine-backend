module Registry.Api.Resource.ActionKey.ActionKeySM where

import Data.Swagger

import Registry.Api.Resource.ActionKey.ActionKeyJM ()
import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Registry.Model.ActionKey.ActionKeyType
import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyDTO
import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyJM ()
import Shared.Common.Util.Swagger

instance ToSchema ActionKeyType

instance ToSchema (ActionKeyDTO ActionKeyType) where
  declareNamedSchema = toSwagger forgottenTokenActionKeyDto
