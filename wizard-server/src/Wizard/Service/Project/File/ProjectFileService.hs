module Wizard.Service.Project.File.ProjectFileService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
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
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectFileDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Project.File.ProjectFile
import Wizard.Model.Project.File.ProjectFileList
import Wizard.Model.Project.Project
import Wizard.S3.Project.ProjectFileS3
import Wizard.Service.Project.Collaboration.ProjectCollaborationService
import Wizard.Service.Project.File.ProjectFileAcl
import Wizard.Service.Project.File.ProjectFileMapper
import Wizard.Service.Project.File.ProjectFileValidation
import Wizard.Service.Project.ProjectAcl
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getProjectFilesPage :: Maybe String -> Maybe U.UUID -> Pageable -> [Sort] -> AppContextM (Page ProjectFileList)
getProjectFilesPage mQuery mProjectUuid pageable sort = do
  case mProjectUuid of
    Just projectUuid -> do
      project <- findProjectByUuid projectUuid
      checkViewPermissionToProject project.visibility project.sharing project.permissions
    Nothing -> checkPermission _PRJ_FILE_PERM
  findProjectFilesPage mQuery mProjectUuid pageable sort

createProjectFile :: U.UUID -> U.UUID -> FileCreateDTO -> AppContextM ProjectFileList
createProjectFile projectUuid questionUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkViewPermissionToProject project.visibility project.sharing project.permissions
    uuid <- liftIO generateUuid
    mCurrentUser <- asks currentUser
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    let projectFile = fromFileCreateDTO reqDto uuid projectUuid mCurrentUser tenantUuid now
    validateProjectFile project questionUuid projectFile
    insertProjectFile projectFile
    putFile projectUuid uuid reqDto.contentType reqDto.content
    addFile projectUuid (toSimple projectFile)
    return $ toList projectFile project mCurrentUser

cloneProjectFiles :: U.UUID -> U.UUID -> AppContextM [(ProjectFile, ProjectFile)]
cloneProjectFiles oldProjectUuid newProjectUuid = do
  runInTransaction $ do
    oldFiles <- findProjectFilesByProject oldProjectUuid
    traverse
      ( \oldFile -> do
          contentAction <- retrieveFileConduitAction oldProjectUuid oldFile.uuid
          newFileUuid <- liftIO generateUuid
          let newFile = oldFile {uuid = newFileUuid, projectUuid = newProjectUuid}
          let contentDisposition = f' "attachment;filename=\"%s\"" [trim newFile.fileName]
          insertProjectFile newFile
          putFileConduit newProjectUuid newFile.uuid newFile.contentType contentDisposition contentAction
          return (oldFile, newFile)
      )
      oldFiles

downloadProjectFile :: U.UUID -> U.UUID -> AppContextM TemporaryFileDTO
downloadProjectFile projectUuid fileUuid = do
  runInTransaction $ do
    projectFile <- findProjectFileByUuid fileUuid
    checkViewPermissionToFile projectUuid
    contentAction <- retrieveFileConduitAction projectUuid fileUuid
    mCurrentUserUuid <- getCurrentUserUuid
    url <- createTemporaryFileConduit projectFile.fileName "application/octet-stream" mCurrentUserUuid contentAction
    return $ TemporaryFileMapper.toDTO url projectFile.contentType

deleteProjectFile :: U.UUID -> U.UUID -> AppContextM ()
deleteProjectFile projectUuid fileUuid = do
  runInTransaction $ do
    _ <- findProjectFileByUuid fileUuid
    checkEditPermissionToFile projectUuid
    void $ deleteProjectFileByUuid fileUuid
