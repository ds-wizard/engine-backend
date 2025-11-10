module Wizard.Api.Handler.DocumentTemplateDraft.File.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO
import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateFileChangeDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> "files"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)

list_POST
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateFileChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateFile)
list_POST mTokenHeader mServerUrl reqDto tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createFile tmlId reqDto
