module Wizard.Api.Handler.DocumentTemplateDraft.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> Capture "documentTemplateId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDetail)

detail_GET
  :: Maybe String -> Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateDraftDetail)
detail_GET mTokenHeader mServerUrl tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getDraft tmlId
