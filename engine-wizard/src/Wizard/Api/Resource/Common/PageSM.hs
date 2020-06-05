module Wizard.Api.Resource.Common.PageSM where

import Data.Swagger
import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Common.PageMetadataSM ()
import Shared.Database.Migration.Development.Common.Data.Pages
import Shared.Model.Common.Page
import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema (Page QuestionnaireDTO) where
  declareNamedSchema = simpleToSchema (Page "questionnaires" pageMetadata [questionnaire1Dto])
