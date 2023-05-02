module Wizard.Api.Resource.User.UserProfileChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileChangeJM ()
import Wizard.Api.Resource.User.UserSubmissionPropsSM ()
import Wizard.Database.Migration.Development.User.Data.Users

instance ToSchema UserProfileChangeDTO where
  declareNamedSchema = toSwagger userIsaacProfileChange
