module Wizard.Api.Resource.User.UserSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Database.Migration.Development.User.Data.Users

instance ToSchema UserSuggestionDTO where
  declareNamedSchema = toSwagger userAlbertSuggestion
