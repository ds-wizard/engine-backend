module Wizard.Api.Resource.Locale.LocaleChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleChangeJM ()
import Wizard.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleChangeDTO where
  declareNamedSchema = toSwagger localeNlChangeDto
