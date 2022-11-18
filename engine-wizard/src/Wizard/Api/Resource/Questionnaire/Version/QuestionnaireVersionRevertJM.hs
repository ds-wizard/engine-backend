module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO

instance FromJSON QuestionnaireVersionRevertDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireVersionRevertDTO where
  toJSON = genericToJSON jsonOptions
