module Wizard.Api.Resource.Config.ClientConfigJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.SimpleFeatureJM ()

instance FromJSON ClientConfigDTO where
  parseJSON = simpleParseJSON "_clientConfigDTO"

instance ToJSON ClientConfigDTO where
  toJSON = simpleToJSON "_clientConfigDTO"

instance FromJSON ClientConfigAuthDTO where
  parseJSON = simpleParseJSON "_clientConfigAuthDTO"

instance ToJSON ClientConfigAuthDTO where
  toJSON = simpleToJSON "_clientConfigAuthDTO"

instance FromJSON ClientConfigAuthExternalDTO where
  parseJSON = simpleParseJSON "_clientConfigAuthExternalDTO"

instance ToJSON ClientConfigAuthExternalDTO where
  toJSON = simpleToJSON "_clientConfigAuthExternalDTO"

instance FromJSON ClientConfigAuthExternalServiceDTO where
  parseJSON = simpleParseJSON "_clientConfigAuthExternalServiceDTO"

instance ToJSON ClientConfigAuthExternalServiceDTO where
  toJSON = simpleToJSON "_clientConfigAuthExternalServiceDTO"

instance FromJSON ClientConfigRegistryDTO where
  parseJSON = simpleParseJSON "_clientConfigRegistryDTO"

instance ToJSON ClientConfigRegistryDTO where
  toJSON = simpleToJSON "_clientConfigRegistryDTO"

instance FromJSON ClientConfigQuestionnaireDTO where
  parseJSON = simpleParseJSON "_clientConfigQuestionnaireDTO"

instance ToJSON ClientConfigQuestionnaireDTO where
  toJSON = simpleToJSON "_clientConfigQuestionnaireDTO"
