module Wizard.Api.Resource.User.UserProfileSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserProfileJM ()
import Wizard.Api.Resource.User.UserSubmissionPropsSM ()
import Wizard.Database.Migration.Development.User.Data.Users

instance ToSchema UserProfileDTO where
  declareNamedSchema = toSwagger userAlbertProfile
