module Wizard.Api.Handler.DocumentTemplateDraft.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Common.EntityCreatedWithIdDTO
import Shared.Common.Api.Resource.Common.EntityCreatedWithIdJM ()
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateDraftCreateDTO
    :> "document-template-drafts"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] EntityCreatedWithIdDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateDraftCreateDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] EntityCreatedWithIdDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createDraft reqDto
