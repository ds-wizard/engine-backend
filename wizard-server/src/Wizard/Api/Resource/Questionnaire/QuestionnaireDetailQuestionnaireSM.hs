module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireSM where

import Data.Map.Strict as M
import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.Questionnaire
import WizardLib.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels

instance ToSchema QuestionnaireDetailQuestionnaireDTO where
  declareNamedSchema =
    toSwagger $
      QuestionnaireDetailQuestionnaireDTO
        { uuid = questionnaire1.uuid
        , name = questionnaire1.name
        , visibility = questionnaire1.visibility
        , sharing = questionnaire1.sharing
        , packageId = questionnaire1.packageId
        , selectedQuestionTagUuids = questionnaire1.selectedQuestionTagUuids
        , isTemplate = questionnaire1.isTemplate
        , knowledgeModel = km1
        , replies = fReplies
        , labels = fLabels
        , phaseUuid = Just . u' $ "4b376e49-1589-429b-9590-c654378f0bd5"
        , migrationUuid = Nothing
        , permissions = [qtn1AlbertEditQtnPermDto]
        , commentCounts =
            M.fromList
              [
                ( "4f61fdfa-ce82-41b5-a1e6-218beaf41660.0dc58313-eb80-4f74-a8c1-347b644665d5"
                , M.fromList [(u' "f1de85a9-7f22-4d0c-bc23-3315cc4c85d7", 2)]
                )
              ]
        , questionnaireActionsAvailable = 1
        , questionnaireImportersAvailable = 2
        }
