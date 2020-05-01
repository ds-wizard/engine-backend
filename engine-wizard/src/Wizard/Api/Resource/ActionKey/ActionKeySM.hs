module Wizard.Api.Resource.ActionKey.ActionKeySM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Api.Resource.ActionKey.ActionKeyJM ()
import Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Model.ActionKey.ActionKey

instance ToSchema ActionKeyType

instance ToSchema ActionKeyDTO where
  declareNamedSchema = simpleToSchema forgTokActionKeyDto
