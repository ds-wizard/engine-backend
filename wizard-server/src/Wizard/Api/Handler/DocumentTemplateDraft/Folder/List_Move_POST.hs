module Wizard.Api.Handler.DocumentTemplateDraft.Folder.List_Move_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Folder.DocumentTemplateFolderService

type List_Move_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateFolderMoveDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "folders"
    :> "move"
    :> Verb POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_move_POST
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateFolderMoveDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_move_POST mTokenHeader mServerUrl reqDto documentTemplateId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        moveDraftFolder documentTemplateId reqDto
        return NoContent
