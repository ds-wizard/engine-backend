module Wizard.Api.Resource.Usage.UsageSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Api.Resource.Usage.UsageJM ()
import Wizard.Database.Migration.Development.Usage.Data.Usages

instance ToSchema UsageDTO where
  declareNamedSchema = toSwagger defaultUsage

instance ToSchema UsageEntryDTO where
  declareNamedSchema = toSwagger defaultUsageUsers
