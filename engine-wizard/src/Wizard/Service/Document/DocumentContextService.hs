module Wizard.Service.Document.DocumentContextService where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper)
import Shared.Util.Uuid
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Database.DAO.Level.LevelDAO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Database.DAO.Organization.OrganizationDAO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Document.DocumentContextMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Report.ReportGenerator

createDocumentContext :: String -> AppContextM (Either AppError DocumentContextDTO)
createDocumentContext qtnUuid =
  heFindQuestionnaireById qtnUuid $ \qtn ->
    heFindPackageById (qtn ^. packageId) $ \pkg ->
      heFindMetrics $ \metrics ->
        heFindLevels $ \levels ->
          heFindOrganization $ \org ->
            heCompileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids) $ \km ->
              heCreatedBy (qtn ^. ownerUuid) $ \mCreatedBy -> do
                appConfig <- asks _appContextApplicationConfig
                dmpUuid <- liftIO generateUuid
                now <- liftIO getCurrentTime
                let _level =
                      if appConfig ^. general . levelsEnabled
                        then qtn ^. level
                        else 9999
                report <- generateReport _level metrics km (qtn ^. replies)
                let dmp =
                      fromCreateContextDTO dmpUuid appConfig qtn _level km metrics levels report pkg org mCreatedBy now
                return . Right . toDocumentContextDTO $ dmp
  where
    heCreatedBy mOwnerUuid callback =
      case mOwnerUuid of
        Just ownerUuid -> heFindUserById (U.toString ownerUuid) $ \createdBy -> callback . Just $ createdBy
        Nothing -> callback Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heCreateDocumentContext qtnUuid = createHeeHelper (createDocumentContext qtnUuid)
