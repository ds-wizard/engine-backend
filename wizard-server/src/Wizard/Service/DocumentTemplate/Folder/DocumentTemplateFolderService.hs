module Wizard.Service.DocumentTemplate.Folder.DocumentTemplateFolderService where

import qualified Data.UUID as U

import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

moveDraftFolder :: U.UUID -> DocumentTemplateFolderMoveDTO -> AppContextM ()
moveDraftFolder documentTemplateUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    moveFolder documentTemplateUuid reqDto.current reqDto.new
    touchDocumentTemplateByUuid documentTemplateUuid
    deleteTemporalDocumentsByDocumentTemplateUuid documentTemplateUuid
    return ()

deleteDraftFolder :: U.UUID -> DocumentTemplateFolderDeleteDTO -> AppContextM ()
deleteDraftFolder documentTemplateUuid reqDto =
  runInTransaction $ do
    checkPermission _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION
    deleteFolder documentTemplateUuid reqDto.path
    touchDocumentTemplateByUuid documentTemplateUuid
    deleteTemporalDocumentsByDocumentTemplateUuid documentTemplateUuid
    return ()
