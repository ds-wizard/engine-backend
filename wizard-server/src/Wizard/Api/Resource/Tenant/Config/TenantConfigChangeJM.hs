module Wizard.Api.Resource.Tenant.Config.TenantConfigChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()

instance FromJSON TenantConfigChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmissionChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmissionServiceChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionServiceChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmissionServiceSupportedFormatChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionServiceSupportedFormatChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmissionServiceRequestChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionServiceRequestChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigSubmissionServiceRequestMultipartChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigSubmissionServiceRequestMultipartChangeDTO where
  toJSON = genericToJSON jsonOptions
