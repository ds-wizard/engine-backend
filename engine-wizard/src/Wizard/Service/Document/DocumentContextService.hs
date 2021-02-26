module Wizard.Service.Document.DocumentContextService where

import Control.Lens ((^.))
import Control.Monad (forM)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Common.Lens
import Shared.Util.List
import Shared.Util.Uuid
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Database.DAO.Level.LevelDAO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Document.DocumentContextMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireUtils
import Wizard.Service.Report.ReportGenerator
import Wizard.Service.User.UserService

createDocumentContext :: String -> AppContextM DocumentContextDTO
createDocumentContext qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  pkg <- getPackageById (qtn ^. packageId)
  metrics <- findMetrics
  ls <- findLevels
  km <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids)
  mCreatedBy <- forM (fmap U.toString (qtn ^. creatorUuid)) getUserById
  appConfig <- getAppConfig
  serverConfig <- asks _appContextServerConfig
  let org = appConfig ^. organization
  dmpUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let qtnEvents =
        case doc ^. questionnaireEventUuid of
          Just eventUuid -> takeWhileInclusive (\e -> e ^. uuid' /= eventUuid) (qtn ^. events)
          Nothing -> qtn ^. events
  qtnCtn <- compileQuestionnairePreview qtnEvents
  let _level =
        if appConfig ^. questionnaire . levels . enabled
          then qtnCtn ^. level
          else 9999
  report <- generateReport _level metrics km (M.toList $ qtnCtn ^. replies)
  qtnVersionDtos <- traverse enhanceQuestionnaireVersion (qtn ^. versions)
  return $
    toDocumentContextDTO
      dmpUuid
      appConfig
      serverConfig
      qtn
      qtnCtn
      qtnVersionDtos
      _level
      km
      metrics
      ls
      report
      pkg
      org
      mCreatedBy
      now
