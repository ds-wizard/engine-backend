module Wizard.Service.Template.File.TemplateFileService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.UUID as U

import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Model.Template.Template
import Shared.Util.Uuid
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.Acl.AclService
import Wizard.Service.Template.File.TemplateFileMapper
import Wizard.Service.Template.TemplateValidation

getTemplateFiles :: String -> AppContextM [TemplateFile]
getTemplateFiles tmlId = do
  checkPermission _TML_PERM
  findTemplateFilesByTemplateId tmlId

getTemplateFile :: U.UUID -> AppContextM TemplateFile
getTemplateFile fileUuid = do
  checkPermission _TML_PERM
  findTemplateFileById (U.toString fileUuid)

createTemplateFile :: String -> TemplateFileChangeDTO -> AppContextM TemplateFile
createTemplateFile tmlId reqDto =
  runInTransaction $ do
    checkPermission _TML_PERM
    validateTemplateFileAndAssetUniqueness Nothing tmlId reqDto.fileName
    fUuid <- liftIO generateUuid
    appUuid <- asks currentAppUuid
    let newFile = fromChangeDTO reqDto tmlId fUuid appUuid
    insertTemplateFile newFile
    deleteTemporalDocumentsByTemplateFileId (U.toString fUuid)
    return newFile

modifyTemplateFile :: String -> TemplateFileChangeDTO -> AppContextM TemplateFile
modifyTemplateFile fileUuid reqDto =
  runInTransaction $ do
    checkPermission _TML_PERM
    file <- findTemplateFileById fileUuid
    validateTemplateFileAndAssetUniqueness (Just file.uuid) file.templateId reqDto.fileName
    let updatedFile = fromChangeDTO reqDto file.templateId file.uuid file.appUuid
    updateTemplateFileById updatedFile
    deleteTemporalDocumentsByTemplateFileId fileUuid
    return updatedFile

deleteTemplateFile :: String -> AppContextM ()
deleteTemplateFile fileUuid =
  runInTransaction $ do
    checkPermission _TML_PERM
    file <- findTemplateFileById fileUuid
    deleteTemplateFileById (U.toString file.uuid)
    deleteTemporalDocumentsByTemplateFileId fileUuid
    return ()
