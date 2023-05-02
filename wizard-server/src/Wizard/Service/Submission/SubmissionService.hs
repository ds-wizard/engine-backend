module Wizard.Service.Submission.SubmissionService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.Submission.SubmissionServiceSimpleDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Api.Resource.User.UserSubmissionPropsDTO
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
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Document.DocumentAcl
import Wizard.Service.Submission.SubmissionAcl
import Wizard.Service.Submission.SubmissionMapper
import Wizard.Service.Submission.SubmissionUtil
import Wizard.Service.User.UserProfileService

getAvailableServicesForSubmission :: U.UUID -> AppContextM [SubmissionServiceSimpleDTO]
getAvailableServicesForSubmission docUuid = do
  checkIfSubmissionIsEnabled
  checkPermissionToSubmission docUuid
  appConfig <- getAppConfig
  doc <- findDocumentByUuid docUuid
  checkEditPermissionToDoc doc.questionnaireUuid
  return . fmap toSubmissionServiceSimpleDTO . filter (filterService doc) $ appConfig.submission.services
  where
    filterService :: Document -> AppConfigSubmissionService -> Bool
    filterService doc service = any (filterServiceFormat doc) $ service.supportedFormats
    filterServiceFormat :: Document -> AppConfigSubmissionServiceSupportedFormat -> Bool
    filterServiceFormat doc supportedFormat =
      (supportedFormat.templateId == doc.documentTemplateId) && (supportedFormat.formatUuid == doc.formatUuid)

getSubmissionsForDocument :: U.UUID -> AppContextM [SubmissionDTO]
getSubmissionsForDocument docUuid = do
  checkIfSubmissionIsEnabled
  doc <- findDocumentByUuid docUuid
  checkViewPermissionToDoc doc.questionnaireUuid
  submissions <- findSubmissionsByDocumentUuid docUuid
  traverse enhanceSubmission submissions

submitDocument :: U.UUID -> SubmissionCreateDTO -> AppContextM SubmissionDTO
submitDocument docUuid reqDto =
  runInTransaction $ do
    checkIfSubmissionIsEnabled
    checkPermissionToSubmission docUuid
    appConfig <- getAppConfig
    case L.find (\s -> s.sId == reqDto.serviceId) appConfig.submission.services of
      Just definition -> do
        doc <- findDocumentByUuid docUuid
        checkEditPermissionToDoc doc.questionnaireUuid
        docContent <- retrieveDocumentContent docUuid
        userProps <- getUserProps definition
        sub <- createSubmission docUuid reqDto
        response <- uploadDocument definition.request userProps docContent
        let updatedSub =
              case response of
                Right mLocation ->
                  sub
                    { state = DoneSubmissionState
                    , location = mLocation
                    }
                  :: Submission
                Left error ->
                  sub
                    { state = ErrorSubmissionState
                    , returnedData = Just error
                    }
                  :: Submission
        savedSubmission <- updateSubmissionByUuid updatedSub
        enhanceSubmission savedSubmission
      Nothing -> throwError . UserError $ _ERROR_VALIDATION__SUBMISSION_DEFINITION_ABSENCE reqDto.serviceId
  where
    getUserProps definition = do
      mUser <- asks currentUser
      case mUser of
        Just user -> do
          profile <- getUserProfile user.uuid
          let mUserProps = L.find (\p -> p.sId == definition.sId) profile.submissionProps
          return $
            case mUserProps of
              Just p -> p.values
              Nothing -> M.empty
        Nothing -> return M.empty

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfSubmissionIsEnabled = checkIfAppFeatureIsEnabled "Submission" (\c -> c.submission.enabled)

createSubmission :: U.UUID -> SubmissionCreateDTO -> AppContextM Submission
createSubmission docUuid reqDto = do
  sUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  appUuid <- asks currentAppUuid
  currentUser <- getCurrentUser
  let sub = fromCreate sUuid reqDto.serviceId docUuid appUuid currentUser.uuid now
  insertSubmission sub
  return sub
