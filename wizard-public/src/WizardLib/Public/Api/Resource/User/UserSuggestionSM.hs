module WizardLib.Public.Api.Resource.User.UserSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()
import WizardLib.Public.Database.Migration.Development.User.Data.Users

instance ToSchema UserSuggestionDTO where
  declareNamedSchema = toSwagger userAlbertSuggestion
