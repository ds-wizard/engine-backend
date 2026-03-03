module Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.DocumentTemplate.DocumentTemplateValidation
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileMapper

getFiles :: U.UUID -> AppContextM [DocumentTemplateFileList]
getFiles dtUuid = do
  checkPermission _DOC_TML_WRITE_PERM
  findFileListsByDocumentTemplateUuid dtUuid

getFile :: U.UUID -> AppContextM DocumentTemplateFile
getFile fileUuid = do
  checkPermission _DOC_TML_WRITE_PERM
  findFileByUuid fileUuid

createFile :: U.UUID -> DocumentTemplateFileChangeDTO -> AppContextM DocumentTemplateFile
createFile dtUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    validateFileAndAssetUniqueness Nothing dtUuid reqDto.fileName
    fUuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    let newFile = fromChangeDTO reqDto dtUuid fUuid tenantUuid now now
    insertFile newFile
    touchDocumentTemplateByUuid newFile.documentTemplateUuid
    deleteTemporalDocumentsByFileUuid fUuid
    return newFile

modifyFile :: U.UUID -> DocumentTemplateFileChangeDTO -> AppContextM DocumentTemplateFile
modifyFile fileUuid reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    file <- findFileByUuid fileUuid
    validateFileAndAssetUniqueness (Just file.uuid) file.documentTemplateUuid reqDto.fileName
    now <- liftIO getCurrentTime
    let updatedFile = fromChangeDTO reqDto file.documentTemplateUuid file.uuid file.tenantUuid file.createdAt now
    updateFileByUuid updatedFile
    touchDocumentTemplateByUuid updatedFile.documentTemplateUuid
    deleteTemporalDocumentsByFileUuid fileUuid
    return updatedFile

modifyFileContent :: U.UUID -> String -> AppContextM DocumentTemplateFile
modifyFileContent fileUuid content =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    file <- findFileByUuid fileUuid
    now <- liftIO getCurrentTime
    let updatedFile = fromContentChangeDTO file content now
    updateFileByUuid updatedFile
    touchDocumentTemplateByUuid updatedFile.documentTemplateUuid
    deleteTemporalDocumentsByFileUuid fileUuid
    return updatedFile

duplicateFile :: U.UUID -> DocumentTemplateFile -> AppContextM DocumentTemplateFile
duplicateFile newDtUuid file = do
  aUuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let updatedFile = fromDuplicateDTO file newDtUuid aUuid now
  insertFile updatedFile
  touchDocumentTemplateByUuid updatedFile.documentTemplateUuid
  return updatedFile

deleteFile :: U.UUID -> AppContextM ()
deleteFile fileUuid =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    file <- findFileByUuid fileUuid
    deleteFileById file.uuid
    touchDocumentTemplateByUuid file.documentTemplateUuid
    deleteTemporalDocumentsByFileUuid fileUuid
    return ()
