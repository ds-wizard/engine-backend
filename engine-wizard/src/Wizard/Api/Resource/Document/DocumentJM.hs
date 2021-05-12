module Wizard.Api.Resource.Document.DocumentJM where

import Data.Aeson

import Shared.Model.Template.TemplateJM ()
import Shared.Util.JSON
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Model.Document.Document

instance FromJSON DocumentState

instance ToJSON DocumentState

instance FromJSON DocumentMetadata where
  parseJSON = simpleParseJSON "_documentMetadata"

instance ToJSON DocumentMetadata where
  toJSON = simpleToJSON "_documentMetadata"

instance FromJSON DocumentDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DocumentDTO where
  toJSON = genericToJSON simpleOptions
