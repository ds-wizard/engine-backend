module Wizard.Service.Document.DocumentContextService
  ( createDocumentContext
  ) where

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
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Document.DocumentContextJM ()
import Wizard.Model.Questionnaire.QuestionnaireVersion
import Wizard.Service.App.AppHelper
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Document.DocumentContextMapper
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireUtils
import Wizard.Service.Report.ReportGenerator

createDocumentContext :: Document -> AppContextM DocumentContext
createDocumentContext doc = do
  qtn <- findQuestionnaireById . U.toString $ doc ^. questionnaireUuid
  pkg <- getPackageById (qtn ^. packageId)
  km <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedQuestionTagUuids)
  mCreatedBy <- forM (fmap U.toString (qtn ^. creatorUuid)) findUserById
  appConfig <- getAppConfig
  serverConfig <- asks _appContextServerConfig
  clientUrl <- getAppClientUrl
  let org = appConfig ^. organization
  dmpUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let qtnEvents =
        case doc ^. questionnaireEventUuid of
          Just eventUuid -> takeWhileInclusive (\e -> e ^. uuid' /= eventUuid) (qtn ^. events)
          Nothing -> qtn ^. events
  qtnCtn <- compileQuestionnairePreview qtnEvents
  report <- generateReport (qtnCtn ^. phaseUuid) km (M.toList $ qtnCtn ^. replies)
  let qtnVersion =
        case doc ^. questionnaireEventUuid of
          (Just eventUuid) -> findQuestionnaireVersionUuid eventUuid (qtn ^. versions)
          _ -> Nothing
  qtnVersionDtos <- traverse enhanceQuestionnaireVersion (qtn ^. versions)
  return $
    toDocumentContext
      dmpUuid
      serverConfig
      clientUrl
      qtn
      qtnCtn
      qtnVersion
      qtnVersionDtos
      (qtn ^. projectTags)
      (qtnCtn ^. phaseUuid)
      km
      report
      pkg
      org
      mCreatedBy
      now

-- --------------------------------
-- PRIVATE
-- --------------------------------
findQuestionnaireVersionUuid :: U.UUID -> [QuestionnaireVersion] -> Maybe U.UUID
findQuestionnaireVersionUuid _ [] = Nothing
findQuestionnaireVersionUuid desiredEventUuid (version:rest)
  | desiredEventUuid == (version ^. eventUuid) = Just $ version ^. uuid
  | otherwise = findQuestionnaireVersionUuid desiredEventUuid rest
