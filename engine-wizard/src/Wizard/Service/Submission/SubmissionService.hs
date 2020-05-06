module Wizard.Service.Submission.SubmissionService where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Integration.Http.Submission.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Document.Document
import Wizard.Service.Common
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Document.DocumentService
import Wizard.Service.Submission.SubmissionMapper
import Wizard.Service.User.UserProfileService

getAvailableServicesForSubmission :: String -> AppContextM [SubmissionServiceSimpleDTO]
getAvailableServicesForSubmission docUuid = do
  checkPermissionToDocument docUuid
  appConfig <- getAppConfig
  doc <- findDocumentById docUuid
  return . fmap toSubmissionServiceSimpleDTO . filter (filterService doc) $ appConfig ^. submission . services
  where
    filterService :: Document -> AppConfigSubmissionService -> Bool
    filterService doc service = not . null . filter (filterServiceFormat doc) $ service ^. supportedFormats
    filterServiceFormat :: Document -> AppConfigSubmissionServiceSupportedFormat -> Bool
    filterServiceFormat doc supportedFormat =
      (supportedFormat ^. templateUuid == doc ^. templateUuid) && (supportedFormat ^. formatUuid == doc ^. formatUuid)

submitDocument :: SubmissionCreateDTO -> AppContextM SubmissionDTO
submitDocument reqDto = do
  checkIfSubmissionIsEnabled
  appConfig <- getAppConfig
  case L.find (\s -> s ^. sId == (reqDto ^. serviceId)) (appConfig ^. submission . services) of
    Just definition -> do
      checkPermissionToDocument (U.toString $ reqDto ^. docUuid)
      docContent <- findDocumentContent (U.toString $ reqDto ^. docUuid)
      user <- getCurrentUser
      profile <- getUserProfile (U.toString $ user ^. uuid)
      let mUserProps = L.find (\p -> (p ^. sId) == (definition ^. sId)) (profile ^. submissionProps)
      let userProps =
            case mUserProps of
              Just p -> p ^. values
              Nothing -> M.empty
      uploadDocument (definition ^. request) userProps docContent
    Nothing -> throwError . UserError $ _ERROR_VALIDATION__SUBMISSION_DEFINITION_ABSENCE (reqDto ^. serviceId)

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfSubmissionIsEnabled = checkIfAppFeatureIsEnabled "Submission" (submission . enabled)
