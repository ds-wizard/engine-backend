module Wizard.Api.Resource.User.UserSubmissionPropListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.UserSubmissionPropListJM ()
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.UserSubmissionPropList

instance ToSchema UserSubmissionPropList where
  declareNamedSchema = toSwagger userAlbertApiTokenList
