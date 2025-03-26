module WizardLib.Public.Api.Resource.User.UserLocaleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.User.UserLocaleDTO
import WizardLib.Public.Api.Resource.User.UserLocaleJM ()
import WizardLib.Public.Database.Migration.Development.User.Data.Users

instance ToSchema UserLocaleDTO where
  declareNamedSchema = toSwagger userLocale
