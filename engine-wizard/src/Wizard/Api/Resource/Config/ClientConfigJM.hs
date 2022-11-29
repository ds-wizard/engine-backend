module Wizard.Api.Resource.Config.ClientConfigJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.SimpleFeatureJM ()
import Wizard.Api.Resource.Locale.LocaleJM ()

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
