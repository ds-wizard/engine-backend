module Wizard.Api.Resource.User.UserChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.User.UserChangeDTO
import Wizard.Api.Resource.User.UserChangeJM ()
import Wizard.Database.Migration.Development.User.Data.Users

instance ToSchema UserChangeDTO where
  declareNamedSchema = simpleToSchema userIsaacChange
