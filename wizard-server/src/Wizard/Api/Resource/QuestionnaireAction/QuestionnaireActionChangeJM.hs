module Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeDTO

instance FromJSON QuestionnaireActionChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireActionChangeDTO where
  toJSON = genericToJSON jsonOptions
