module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.Questionnaire

instance ToSchema QuestionnaireDetailDTO where
  declareNamedSchema =
    toSwagger $
      QuestionnaireDetailDTO
        { uuid = questionnaire1.uuid
        , name = questionnaire1.name
        , visibility = questionnaire1.visibility
        , sharing = questionnaire1.sharing
        , packageId = questionnaire1.packageId
        , isTemplate = questionnaire1.isTemplate
        , migrationUuid = Nothing
        , permissions = [qtn1AlbertEditQtnPermDto]
        , fileCount = 0
        }
