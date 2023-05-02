module Wizard.Api.Resource.Document.DocumentJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleJM ()
import Wizard.Model.Document.Document
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromJSON DocumentState

instance ToJSON DocumentState

instance FromJSON DocumentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentDTO where
  toJSON = genericToJSON jsonOptions
