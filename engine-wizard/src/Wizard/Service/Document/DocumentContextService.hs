module Wizard.Service.Document.DocumentContextService where

import Control.Lens ((^.))
import Control.Monad (forM)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
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

createDocumentContext :: String -> AppContextM DocumentContextDTO
createDocumentContext qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  pkg <- findPackageById (qtn ^. packageId)
  metrics <- findMetrics
  levels <- findLevels
  org <- findOrganization
  km <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids)
  mCreatedBy <- forM (fmap U.toString (qtn ^. ownerUuid)) findUserById
  appConfig <- asks _appContextApplicationConfig
  dmpUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let _level =
        if appConfig ^. general . levelsEnabled
          then qtn ^. level
          else 9999
  report <- generateReport _level metrics km (qtn ^. replies)
  let dmp = fromCreateContextDTO dmpUuid appConfig qtn _level km metrics levels report pkg org mCreatedBy now
  return . toDocumentContextDTO $ dmp
