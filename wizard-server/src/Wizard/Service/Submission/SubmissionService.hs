module Wizard.Service.Submission.SubmissionService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Wizard.Integration.Http.Submission.Runner
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Document.Document
import Wizard.Model.Submission.Submission
import Wizard.Model.Submission.SubmissionList
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import Wizard.Model.User.UserSubmissionPropList
import Wizard.S3.Document.DocumentS3
import Wizard.Service.Common
import Wizard.Service.Document.DocumentAcl
import Wizard.Service.Submission.SubmissionAcl
import Wizard.Service.Submission.SubmissionMapper
import Wizard.Service.User.Profile.UserProfileService
import Wizard.Service.User.UserMapper (toSuggestion')

getAvailableServicesForSubmission :: U.UUID -> AppContextM [TenantConfigSubmissionServiceSimple]
getAvailableServicesForSubmission docUuid = do
  checkIfSubmissionIsEnabled
  doc <- findDocumentByUuid docUuid
  checkEditPermissionToSubmission doc
  findTenantConfigSubmissionServicesByDocumentTemplateIdAndFormatUuid doc.documentTemplateId doc.formatUuid

getSubmissionsForDocument :: U.UUID -> AppContextM [SubmissionList]
getSubmissionsForDocument docUuid = do
  checkIfSubmissionIsEnabled
  doc <- findDocumentByUuid docUuid
  checkViewPermissionToDoc doc.questionnaireUuid
  findSubmissionsByDocumentUuid docUuid

submitDocument :: U.UUID -> SubmissionCreateDTO -> AppContextM SubmissionList
submitDocument docUuid reqDto =
  runInTransaction $ do
    checkIfSubmissionIsEnabled
    doc <- findDocumentByUuid docUuid
    checkEditPermissionToSubmission doc
    tcSubmission <- findTenantConfigSubmissionServiceByServiceId reqDto.serviceId
    docContent <- retrieveDocumentContent docUuid
    userProps <- getUserProps tcSubmission
    sub <- createSubmission docUuid reqDto
    response <- uploadDocument tcSubmission.request userProps docContent
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
    currentUser <- getCurrentUser
    return $ toList savedSubmission tcSubmission (toSuggestion' currentUser)
  where
    getUserProps tcSubmission = do
      mUser <- asks currentUser
      case mUser of
        Just user -> do
          submissionProps <- getUserProfileSubmissionProps user.uuid
          let mUserProps = L.find (\p -> p.sId == tcSubmission.sId) submissionProps
          return $
            case mUserProps of
              Just p -> p.values
              Nothing -> M.empty
        Nothing -> return M.empty

-- --------------------------------
-- PRIVATE
-- --------------------------------
checkIfSubmissionIsEnabled = checkIfTenantFeatureIsEnabled "Submission" findTenantConfigSubmission (.enabled)

createSubmission :: U.UUID -> SubmissionCreateDTO -> AppContextM Submission
createSubmission docUuid reqDto = do
  sUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  tenantUuid <- asks currentTenantUuid
  currentUser <- getCurrentUser
  let sub = fromCreate sUuid reqDto.serviceId docUuid tenantUuid currentUser.uuid now
  insertSubmission sub
  return sub
