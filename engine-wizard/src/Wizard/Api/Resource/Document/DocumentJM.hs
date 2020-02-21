module Wizard.Api.Resource.Document.DocumentJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Model.Document.Document

instance FromJSON DocumentState

instance ToJSON DocumentState

instance FromJSON DocumentDTO where
  parseJSON = simpleParseJSON "_documentDTO"

instance ToJSON DocumentDTO where
  toJSON = simpleToJSON "_documentDTO"
