module Wizard.Api.Resource.Tenant.Config.TenantConfigSubmissionServiceSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigJM ()

instance FromJSON TenantConfigSubmissionServiceSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionServiceSimple where
  toJSON = genericToJSON jsonOptions
