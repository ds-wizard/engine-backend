module Wizard.Api.Resource.Document.DocumentJM where

import Data.Aeson

import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Model.Document.Document
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON DocumentState

instance ToJSON DocumentState

instance FromJSON DocumentDTO where
  parseJSON = simpleParseJSON "_documentDTO"

instance ToJSON DocumentDTO where
  toJSON = simpleToJSON "_documentDTO"
