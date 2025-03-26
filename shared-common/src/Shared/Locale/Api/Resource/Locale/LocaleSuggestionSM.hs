module Shared.Locale.Api.Resource.Locale.LocaleSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.Locale.Api.Resource.Locale.LocaleSuggestionJM ()
import Shared.Locale.Database.Migration.Development.Locale.Data.Locales
import Shared.Locale.Model.Locale.LocaleSuggestion

instance ToSchema LocaleSuggestion where
  declareNamedSchema = toSwagger localeNlSuggestion
