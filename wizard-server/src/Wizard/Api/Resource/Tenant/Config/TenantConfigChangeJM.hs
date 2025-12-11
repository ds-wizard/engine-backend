module Wizard.Api.Resource.Tenant.Config.TenantConfigChangeJM where

import Data.Aeson

import Shared.Common.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeJM ()

instance FromJSON TenantConfigChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigOrganizationChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigOrganizationChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAuthenticationChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAuthenticationChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAuthenticationExternalChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAuthenticationExternalChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigAuthenticationExternalServiceChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigAuthenticationExternalServiceChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigPrivacyAndSupportChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigPrivacyAndSupportChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigDashboardAndLoginScreenChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigDashboardAndLoginScreenChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigRegistryChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigRegistryChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigKnowledgeModelChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigKnowledgeModelChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigKnowledgeModelPublicChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigKnowledgeModelPublicChangeDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantConfigProjectChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigProjectChangeDTO where
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

instance FromJSON TenantConfigFeaturesChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigFeaturesChangeDTO where
  toJSON = genericToJSON jsonOptions
