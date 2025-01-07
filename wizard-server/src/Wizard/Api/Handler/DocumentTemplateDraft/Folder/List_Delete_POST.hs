module Wizard.Api.Handler.DocumentTemplateDraft.Folder.List_Delete_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Folder.DocumentTemplateFolderService

type List_Delete_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateFolderDeleteDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "folders"
    :> "delete"
    :> Verb POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_delete_POST
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateFolderDeleteDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_delete_POST mTokenHeader mServerUrl reqDto documentTemplateId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteDraftFolder documentTemplateId reqDto
        return NoContent
