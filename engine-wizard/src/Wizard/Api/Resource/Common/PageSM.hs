module Wizard.Api.Resource.Common.PageSM where

import Data.Swagger
import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Database.Migration.Development.Common.Data.Pages
import Shared.Model.Common.Page
import Shared.Util.Swagger
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema (Page QuestionnaireDTO) where
  declareNamedSchema = simpleToSchema' "_page" (Page "questionnaires" pageMetadata [questionnaire1Dto])

instance ToSchema (Page DocumentDTO) where
  declareNamedSchema = simpleToSchema' "_page" (Page "documents" pageMetadata [doc1Dto])
