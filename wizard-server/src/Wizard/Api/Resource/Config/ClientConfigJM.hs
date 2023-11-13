module Wizard.Api.Resource.Config.ClientConfigJM where

import Data.Aeson

import Shared.Common.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import Wizard.Api.Resource.User.UserProfileJM ()

instance FromJSON ClientConfigDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigAuthDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigAuthDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigAuthExternalDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigAuthExternalDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigAuthExternalServiceDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigAuthExternalServiceDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigRegistryDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigRegistryDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigQuestionnaireDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigQuestionnaireDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigCloudDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigCloudDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigLocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigLocaleDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigAdminDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigAdminDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClientConfigModuleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClientConfigModuleDTO where
  toJSON = genericToJSON jsonOptions
