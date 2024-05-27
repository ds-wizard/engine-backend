module WizardLib.Public.Api.Resource.Tenant.TenantSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionDTO

instance FromJSON TenantSuggestionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantSuggestionDTO where
  toJSON = genericToJSON jsonOptions
