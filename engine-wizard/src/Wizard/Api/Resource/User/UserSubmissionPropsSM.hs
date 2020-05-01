module Wizard.Api.Resource.User.UserSubmissionPropsSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
import Wizard.Api.Resource.User.UserSubmissionPropsJM ()
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.User.User

instance ToSchema UserSubmissionProps where
  declareNamedSchema = simpleToSchema' "_userSubmissionPropsDTO" userAlbertApiToken

instance ToSchema UserSubmissionPropsDTO where
  declareNamedSchema = simpleToSchema userAlbertApiTokenDto
