module Wizard.Service.Template.File.TemplateFileService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import qualified Data.UUID as U

import LensesConfig
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

getTemplateFiles :: String -> AppContextM [TemplateFile]
getTemplateFiles tmlId =
  runInTransaction $ do
    checkPermission _TML_PERM
    findTemplateFilesByTemplateId tmlId

getTemplateFile :: String -> AppContextM TemplateFile
getTemplateFile fileUuid =
  runInTransaction $ do
    checkPermission _TML_PERM
    findTemplateFileById fileUuid

createTemplateFile :: String -> TemplateFileChangeDTO -> AppContextM TemplateFile
createTemplateFile tmlId reqDto =
  runInTransaction $ do
    checkPermission _TML_PERM
    fUuid <- liftIO generateUuid
    let newFile = fromChangeDTO reqDto tmlId fUuid
    insertTemplateFile newFile
    deleteTemporalDocumentsByTemplateFileId (U.toString fUuid)
    return newFile

modifyTemplateFile :: String -> TemplateFileChangeDTO -> AppContextM TemplateFile
modifyTemplateFile fileUuid reqDto =
  runInTransaction $ do
    checkPermission _TML_PERM
    file <- findTemplateFileById fileUuid
    let updatedFile = fromChangeDTO reqDto (file ^. templateId) (file ^. uuid)
    updateTemplateFileById updatedFile
    deleteTemporalDocumentsByTemplateFileId fileUuid
    return updatedFile

deleteTemplateFile :: String -> AppContextM ()
deleteTemplateFile fileUuid =
  runInTransaction $ do
    checkPermission _TML_PERM
    file <- findTemplateFileById fileUuid
    deleteTemplateFileById (U.toString $ file ^. uuid)
    deleteTemporalDocumentsByTemplateFileId fileUuid
    return ()
