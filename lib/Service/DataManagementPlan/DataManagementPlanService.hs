module Service.DataManagementPlan.DataManagementPlanService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Common.Error
import Common.Uuid
import Database.DAO.Questionnaire.QuestionnaireDAO
import LensesConfig
import Model.Context.AppContext
import Model.DataManagementPlan.DataManagementPlan
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.Questionnaire.Questionnaire
import Service.DataManagementPlan.Convertor
import Service.DataManagementPlan.DataManagementPlanMapper
import Service.DataManagementPlan.ReplyApplicator

createFilledKM :: Questionnaire -> FilledKnowledgeModel
createFilledKM questionnaire =
  let plainFilledKM = toFilledKM (questionnaire ^. knowledgeModel)
  in runReplyApplicator plainFilledKM (questionnaire ^. replies)

createDataManagementPlan :: String -> AppContextM (Either AppError DataManagementPlanDTO)
createDataManagementPlan qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn -> do
    dmpUuid <- liftIO generateUuid
    let filledKM = createFilledKM $ qtn
    now <- liftIO getCurrentTime
    let dmp =
          DataManagementPlan
          { _dataManagementPlanUuid = dmpUuid
          , _dataManagementPlanQuestionnaireUuid = qtnUuid
          , _dataManagementPlanFilledKnowledgeModel = filledKM
          , _dataManagementPlanCreatedAt = now
          , _dataManagementPlanUpdatedAt = now
          }
    return . Right . toDTO $ dmp
