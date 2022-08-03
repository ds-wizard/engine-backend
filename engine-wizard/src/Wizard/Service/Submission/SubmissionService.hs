module Wizard.Service.Submission.SubmissionService where

import Control.Lens ((.~), (?~), (^.))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Integration.Http.Submission.Runner
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Document.Document
import Wizard.Model.Submission.Submission
import Wizard.S3.Document.DocumentS3
import Wizard.Service.Common
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Document.DocumentAcl
import Wizard.Service.Submission.SubmissionAcl
import Wizard.Service.Submission.SubmissionMapper
import Wizard.Service.Submission.SubmissionUtil
import Wizard.Service.User.UserProfileService

getAvailableServicesForSubmission :: String -> AppContextM [SubmissionServiceSimpleDTO]
getAvailableServicesForSubmission docUuid = do
  checkIfSubmissionIsEnabled
  checkPermissionToSubmission docUuid
  appConfig <- getAppConfig
  doc <- findDocumentById docUuid
  checkEditPermissionToDoc (U.toString $ doc ^. questionnaireUuid)
  return . fmap toSubmissionServiceSimpleDTO . filter (filterService doc) $ appConfig ^. submission . services
  where
    filterService :: Document -> AppConfigSubmissionService -> Bool
    filterService doc service = any (filterServiceFormat doc) $ service ^. supportedFormats
    filterServiceFormat :: Document -> AppConfigSubmissionServiceSupportedFormat -> Bool
    filterServiceFormat doc supportedFormat =
      (supportedFormat ^. templateId == doc ^. templateId) && (supportedFormat ^. formatUuid == doc ^. formatUuid)

getSubmissionsForDocument :: String -> AppContextM [SubmissionDTO]
getSubmissionsForDocument docUuid = do
  checkIfSubmissionIsEnabled
  doc <- findDocumentById docUuid
  checkViewPermissionToDoc (U.toString $ doc ^. questionnaireUuid)
  submissions <- findSubmissionsByDocumentUuid docUuid
  traverse enhanceSubmission submissions

submitDocument :: String -> SubmissionCreateDTO -> AppContextM SubmissionDTO
submitDocument docUuid reqDto =
  runInTransaction $ do
    checkIfSubmissionIsEnabled
    checkPermissionToSubmission docUuid
    appConfig <- getAppConfig
    case L.find (\s -> s ^. sId == (reqDto ^. serviceId)) (appConfig ^. submission . services) of
      Just definition -> do
        doc <- findDocumentById docUuid
        checkEditPermissionToDoc (U.toString $ doc ^. questionnaireUuid)
        docContent <- getDocumentContent docUuid
        userProps <- getUserProps definition
        sub <- createSubmission (u' docUuid) reqDto
        response <- uploadDocument (definition ^. request) userProps docContent
        let updatedSub =
              case response of
                Right mLocation -> (state .~ DoneSubmissionState) . (location .~ mLocation) $ sub
                Left error -> (state .~ ErrorSubmissionState) . (returnedData ?~ error) $ sub
        savedSubmission <- updateSubmissionById updatedSub
        enhanceSubmission savedSubmission
      Nothing -> throwError . UserError $ _ERROR_VALIDATION__SUBMISSION_DEFINITION_ABSENCE (reqDto ^. serviceId)
  where
    getUserProps definition = do
      mUser <- asks _appContextCurrentUser
      case mUser of
        Just user -> do
          profile <- getUserProfile (U.toString $ user ^. uuid)
          let mUserProps = L.find (\p -> (p ^. sId) == (definition ^. sId)) (profile ^. submissionProps)
          return $
            case mUserProps of
              Just p -> p ^. values
              Nothing -> M.empty
        Nothing -> return M.empty

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfSubmissionIsEnabled = checkIfAppFeatureIsEnabled "Submission" (submission . enabled)

createSubmission :: U.UUID -> SubmissionCreateDTO -> AppContextM Submission
createSubmission docUuid reqDto = do
  sUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  appUuid <- asks _appContextAppUuid
  currentUser <- getCurrentUser
  let sub = fromCreate sUuid (reqDto ^. serviceId) docUuid appUuid (currentUser ^. uuid) now
  insertSubmission sub
  return sub
