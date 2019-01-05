module Service.DataManagementPlan.DataManagementPlanService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import Data.Time
import qualified Data.UUID as U

import Api.Resource.DataManagementPlan.DataManagementPlanDTO
import Database.DAO.Level.LevelDAO
import Database.DAO.Metric.MetricDAO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.DAO.User.UserDAO
import LensesConfig
import Model.Context.AppContext
import Model.DataManagementPlan.DataManagementPlan
import Model.Error.Error
import Model.FilledKnowledgeModel.FilledKnowledgeModel
import Model.Questionnaire.Questionnaire
import Service.DataManagementPlan.Convertor
import Service.DataManagementPlan.DataManagementPlanMapper
import Service.DataManagementPlan.ReplyApplicator
import Service.Report.ReportGenerator
import Service.Template.TemplateService
import Util.Uuid

createFilledKM :: Questionnaire -> FilledKnowledgeModel
createFilledKM questionnaire =
  let plainFilledKM = toFilledKM (questionnaire ^. knowledgeModel)
  in runReplyApplicator plainFilledKM (questionnaire ^. replies)

createDataManagementPlan :: String -> AppContextM (Either AppError DataManagementPlanDTO)
createDataManagementPlan qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heFindPackageById (qtn ^. packageId) $ \package ->
      heFindMetrics $ \dmpMetrics ->
        heFindLevels $ \dmpLevels ->
          heFindOrganization $ \organization ->
            heCreatedBy (qtn ^. ownerUuid) $ \mCreatedBy -> do
              dmpUuid <- liftIO generateUuid
              let filledKM = createFilledKM $ qtn
              now <- liftIO getCurrentTime
              dmpReport <- generateReport (qtn ^. level) dmpMetrics filledKM
              let dmp =
                    DataManagementPlan
                    { _dataManagementPlanUuid = dmpUuid
                    , _dataManagementPlanQuestionnaireUuid = qtnUuid
                    , _dataManagementPlanLevel = qtn ^. level
                    , _dataManagementPlanFilledKnowledgeModel = filledKM
                    , _dataManagementPlanMetrics = dmpMetrics
                    , _dataManagementPlanLevels = dmpLevels
                    , _dataManagementPlanReport = dmpReport
                    , _dataManagementPlanPackage = package
                    , _dataManagementPlanOrganization = organization
                    , _dataManagementPlanCreatedBy = mCreatedBy
                    , _dataManagementPlanCreatedAt = now
                    , _dataManagementPlanUpdatedAt = now
                    }
              return . Right . toDTO $ dmp
  where
    heCreatedBy mOwnerUuid callback =
      case mOwnerUuid of
        Just ownerUuid -> heFindUserById (U.toString ownerUuid) $ \createdBy -> callback . Just $ createdBy
        Nothing -> callback Nothing

exportDataManagementPlan :: String -> DataManagementPlanFormat -> AppContextM (Either AppError BS.ByteString)
exportDataManagementPlan qtnUuid format = do
  heCreateDataManagementPlan qtnUuid $ \dmp ->
    case format of
      JSON -> return . Right . encode $ dmp
      otherFormat -> do
        result <- generateTemplateInFormat otherFormat dmp
        return result

-- --------------------------------
-- HELPERS
-- --------------------------------
heCreateDataManagementPlan qtnUuid callback = do
  eDmp <- createDataManagementPlan qtnUuid
  case eDmp of
    Right dmp -> callback dmp
    Left error -> return . Left $ error
