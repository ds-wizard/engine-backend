module Wizard.Api.Resource.ActionKey.ActionKeyTypeSM where

import Data.Swagger

import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyDTO
import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyJM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.ActionKey.ActionKeyTypeJM ()
import Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Model.ActionKey.ActionKeyType

instance ToSchema ActionKeyType

instance ToSchema (ActionKeyDTO ActionKeyType) where
  declareNamedSchema = toSwagger forgTokActionKeyDto
