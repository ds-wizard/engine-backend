module Wizard.Api.Resource.User.UserSubmissionPropSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.UserSubmissionPropJM ()
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.UserSubmissionProp

instance ToSchema UserSubmissionProp where
  declareNamedSchema = toSwagger userAlbertApiToken
