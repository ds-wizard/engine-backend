module Wizard.Service.DocumentTemplate.Folder.DocumentTemplateFolderService where

import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

moveDraftFolder :: String -> DocumentTemplateFolderMoveDTO -> AppContextM ()
moveDraftFolder documentTemplateId reqDto =
  runInTransaction $ do
    checkPermission _DOC_TML_WRITE_PERM
    moveFolder documentTemplateId reqDto.current reqDto.new
    touchDocumentTemplateById documentTemplateId
    deleteTemporalDocumentsByDocumentTemplateId documentTemplateId
    return ()
