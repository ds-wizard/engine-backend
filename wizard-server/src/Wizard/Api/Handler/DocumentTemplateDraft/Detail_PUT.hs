module Wizard.Api.Handler.DocumentTemplateDraft.Detail_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateDraftChangeDTO
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDetail)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateDraftChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDetail)
detail_PUT mTokenHeader mServerUrl reqDto tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyDraft tmlId reqDto
