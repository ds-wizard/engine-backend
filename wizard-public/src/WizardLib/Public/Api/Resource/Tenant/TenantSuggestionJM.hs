module WizardLib.Public.Api.Resource.Tenant.TenantSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.Tenant.TenantSuggestion

instance FromJSON TenantSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantSuggestion where
  toJSON = genericToJSON jsonOptions
