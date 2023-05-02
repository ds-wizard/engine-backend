module Wizard.Service.Document.DocumentUtil where

import Control.Monad.Except (catchError)
import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.DocumentTemplate.DocumentTemplateService
import Wizard.Service.Submission.SubmissionService

enhanceDocument :: Document -> AppContextM DocumentDTO
enhanceDocument doc = do
  appConfig <- getAppConfig
  submissions <-
    if appConfig.submission.enabled
      then getSubmissionsForDocument doc.uuid
      else return []
  tml <- getDocumentTemplateByUuidAndPackageId doc.documentTemplateId Nothing
  mQtn <- catchError (findQuestionnaireSimpleByUuid' doc.questionnaireUuid) (\_ -> return Nothing)
  return $ toDTO doc mQtn submissions tml

filterAlreadyDoneDocument :: String -> U.UUID -> Document -> Bool
filterAlreadyDoneDocument documentTemplateId formatUuid doc =
  (doc.state == DoneDocumentState || doc.state == ErrorDocumentState) && Just doc.documentTemplateId == Just documentTemplateId && Just doc.formatUuid == Just formatUuid

computeHash :: Questionnaire -> QuestionnaireContent -> AppConfig -> Maybe UserDTO -> Int
computeHash qtn qtnCtn appConfig mCurrentUser =
  sum
    [ hash $ qtn.name
    , hash $ qtn.description
    , hash $ qtn.versions
    , hash $ qtn.projectTags
    , hash $ appConfig.organization
    , hash . M.toList $ qtnCtn.replies
    , maybe 0 hash qtnCtn.phaseUuid
    , maybe 0 hash mCurrentUser
    ]
