module Wizard.Api.Resource.Document.DocumentSM where

import Data.Swagger

import Shared.Api.Resource.Template.TemplateSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Model.Document.Document

instance ToSchema DocumentState

instance ToSchema DocumentDTO where
  declareNamedSchema = simpleToSchema doc1Dto
