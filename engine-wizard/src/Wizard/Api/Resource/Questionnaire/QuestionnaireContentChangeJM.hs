module Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO

instance FromJSON QuestionnaireContentChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireContentChangeDTO where
  toJSON = genericToJSON jsonOptions
