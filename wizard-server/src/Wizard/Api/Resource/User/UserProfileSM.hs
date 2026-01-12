module Wizard.Api.Resource.User.UserProfileSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.AesonSM ()
import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.UserProfileJM ()
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.UserProfile

instance ToSchema UserProfile where
  declareNamedSchema = toSwagger userAlbertProfile
