module Wizard.Api.Resource.Config.ClientConfigJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.SimpleFeatureJM ()

instance FromJSON ClientConfigDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ClientConfigDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON ClientConfigAuthDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ClientConfigAuthDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON ClientConfigAuthExternalDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ClientConfigAuthExternalDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON ClientConfigAuthExternalServiceDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ClientConfigAuthExternalServiceDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON ClientConfigRegistryDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ClientConfigRegistryDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON ClientConfigQuestionnaireDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ClientConfigQuestionnaireDTO where
  toJSON = genericToJSON simpleOptions
