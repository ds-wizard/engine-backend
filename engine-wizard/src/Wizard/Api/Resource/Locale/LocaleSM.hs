module Wizard.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Database.Migration.Development.Locale.Data.Locales
import Wizard.Service.Locale.LocaleMapper

instance ToSchema LocaleDTO where
  declareNamedSchema = simpleToSchema (toDTO localeCz)
