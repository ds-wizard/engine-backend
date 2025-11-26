module WizardLib.Public.Api.Resource.User.UserSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()
import WizardLib.Public.Database.Migration.Development.User.Data.Users
import WizardLib.Public.Model.User.UserSuggestion

instance ToSchema UserSuggestion where
  declareNamedSchema = toSwagger userAlbertSuggestion
