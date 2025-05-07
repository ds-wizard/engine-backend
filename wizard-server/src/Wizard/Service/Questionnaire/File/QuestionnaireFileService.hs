module Wizard.Service.Questionnaire.File.QuestionnaireFileService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Service.Acl.AclService
import Shared.Common.Util.String
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.File.FileCreateDTO
import Wizard.Constant.Acl
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireFileDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireFile
import Wizard.Model.Questionnaire.QuestionnaireFileList
import Wizard.Model.Questionnaire.QuestionnaireFileSimple
import Wizard.S3.Questionnaire.QuestionnaireFileS3
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.Questionnaire.File.QuestionnaireFileAcl
import Wizard.Service.Questionnaire.File.QuestionnaireFileMapper
import Wizard.Service.Questionnaire.File.QuestionnaireFileValidation
import Wizard.Service.Questionnaire.QuestionnaireAcl
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getQuestionnaireFilesPage :: Maybe String -> Maybe U.UUID -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireFileList)
getQuestionnaireFilesPage mQuery mQtnUuid pageable sort = do
  case mQtnUuid of
    Just qtnUuid -> do
      qtn <- findQuestionnaireByUuid qtnUuid
      checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    Nothing -> checkPermission _QTN_FILE_PERM
  findQuestionnaireFilesPage mQuery mQtnUuid pageable sort

createQuestionnaireFile :: U.UUID -> U.UUID -> FileCreateDTO -> AppContextM QuestionnaireFileList
createQuestionnaireFile qtnUuid questionUuid reqDto =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    uuid <- liftIO generateUuid
    mCurrentUser <- asks currentUser
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    let qtnFile = fromFileCreateDTO reqDto uuid qtnUuid mCurrentUser tenantUuid now
    validateQuestionnaireFile qtn questionUuid qtnFile
    insertQuestionnaireFile qtnFile
    putFile qtnUuid uuid reqDto.contentType reqDto.content
    addFile qtnUuid (toSimple qtnFile)
    return $ toList qtnFile qtn mCurrentUser

cloneQuestionnaireFiles :: U.UUID -> U.UUID -> AppContextM [(QuestionnaireFile, QuestionnaireFile)]
cloneQuestionnaireFiles oldQtnUuid newQtnUuid = do
  runInTransaction $ do
    oldFiles <- findQuestionnaireFilesByQuestionnaire oldQtnUuid
    traverse
      ( \oldFile -> do
          contentAction <- retrieveFileConduitAction oldQtnUuid oldFile.uuid
          newFileUuid <- liftIO generateUuid
          let newFile = oldFile {uuid = newFileUuid, questionnaireUuid = newQtnUuid}
          let contentDisposition = f' "attachment;filename=\"%s\"" [trim newFile.fileName]
          insertQuestionnaireFile newFile
          putFileConduit newQtnUuid newFile.uuid newFile.contentType contentDisposition contentAction
          return (oldFile, newFile)
      )
      oldFiles

downloadQuestionnaireFile :: U.UUID -> U.UUID -> AppContextM TemporaryFileDTO
downloadQuestionnaireFile qtnUuid fileUuid = do
  runInTransaction $ do
    qtnFile <- findQuestionnaireFileByUuid fileUuid
    checkViewPermissionToFile qtnUuid
    contentAction <- retrieveFileConduitAction qtnUuid fileUuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFileConduit qtnFile.fileName "application/octet-stream" mCurrentUserUuid contentAction
    return $ TemporaryFileMapper.toDTO url qtnFile.contentType

deleteQuestionnaireFilesByQuestionnaireUuid :: U.UUID -> AppContextM ()
deleteQuestionnaireFilesByQuestionnaireUuid qtnUuid = do
  runInTransaction $ do
    files <- findQuestionnaireFilesSimpleByQuestionnaire qtnUuid
    checkEditPermissionToFile qtnUuid
    traverse_ (\file -> deleteQuestionnaireFileByUuid file.uuid) files

deleteQuestionnaireFile :: U.UUID -> U.UUID -> AppContextM ()
deleteQuestionnaireFile qtnUuid fileUuid = do
  runInTransaction $ do
    _ <- findQuestionnaireFileByUuid fileUuid
    checkEditPermissionToFile qtnUuid
    void $ deleteQuestionnaireFileByUuid fileUuid
