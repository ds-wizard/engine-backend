module Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFileList
import Shared.Util.Uuid
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.Acl.AclService
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileMapper

getFiles :: String -> AppContextM [DocumentTemplateFileList]
getFiles tmlId = do
  checkPermission _DOC_TML_WRITE_PERM
  findFileListsByDocumentTemplateId tmlId

getFile :: U.UUID -> AppContextM DocumentTemplateFile
getFile fileUuid = do
  checkPermission _DOC_TML_WRITE_PERM
  findFileById fileUuid

createFile :: String -> DocumentTemplateFileChangeDTO -> AppContextM DocumentTemplateFile
createFile tmlId reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    validateFileAndAssetUniqueness Nothing tmlId reqDto.fileName
    fUuid <- liftIO generateUuid
    appUuid <- asks currentAppUuid
    now <- liftIO getCurrentTime
    let newFile = fromChangeDTO reqDto tmlId fUuid appUuid now now
    insertFile newFile
    deleteTemporalDocumentsByFileUuid fUuid
    return newFile

modifyFile :: U.UUID -> DocumentTemplateFileChangeDTO -> AppContextM DocumentTemplateFile
modifyFile fileUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    file <- findFileById fileUuid
    validateFileAndAssetUniqueness (Just file.uuid) file.documentTemplateId reqDto.fileName
    now <- liftIO getCurrentTime
    let updatedFile = fromChangeDTO reqDto file.documentTemplateId file.uuid file.appUuid file.createdAt now
    updateFileById updatedFile
    deleteTemporalDocumentsByFileUuid fileUuid
    return updatedFile

modifyFileContent :: U.UUID -> String -> AppContextM DocumentTemplateFile
modifyFileContent fileUuid content =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    file <- findFileById fileUuid
    now <- liftIO getCurrentTime
    let updatedFile = fromContentChangeDTO file content now
    updateFileById updatedFile
    deleteTemporalDocumentsByFileUuid fileUuid
    return updatedFile

duplicateFile :: String -> DocumentTemplateFile -> AppContextM DocumentTemplateFile
duplicateFile newTemplateId file = do
  aUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let updatedFile = fromDuplicateDTO file newTemplateId aUuid now
  insertFile updatedFile
  return updatedFile

deleteFile :: U.UUID -> AppContextM ()
deleteFile fileUuid =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    file <- findFileById fileUuid
    deleteFileById file.uuid
    deleteTemporalDocumentsByFileUuid fileUuid
    return ()
