module Wizard.Service.Document.DocumentUtils where

import Control.Lens ((^.))
import Control.Monad.Except (catchError)
import qualified Data.UUID as U

import LensesConfig hiding (hash)
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Submission.SubmissionService
import Wizard.Service.Template.TemplateService

enhanceDocument :: Document -> AppContextM DocumentDTO
enhanceDocument doc = do
  appConfig <- getAppConfig
  submissions <-
    if appConfig ^. submission . enabled
      then getSubmissionsForDocument (U.toString $ doc ^. uuid)
      else return []
  tml <- getTemplateByUuidAndPackageId (doc ^. templateId) Nothing
  mQtn <- catchError (findQuestionnaireSimpleById' (U.toString $ doc ^. questionnaireUuid)) (\_ -> return Nothing)
  return $ toDTO doc mQtn submissions tml
