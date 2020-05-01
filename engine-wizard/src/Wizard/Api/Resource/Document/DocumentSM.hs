module Wizard.Api.Resource.Document.DocumentSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Api.Resource.Template.TemplateSM ()
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentMapper

instance ToSchema DocumentState

instance ToSchema DocumentDTO where
  declareNamedSchema = simpleToSchema (toDTO doc1 (Just questionnaire1Dto) commonWizardTemplate)
