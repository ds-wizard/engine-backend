module Wizard.Api.Handler.DocumentTemplate.Detail_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.DocumentTemplateService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> Capture "documentTemplateId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)

detail_GET
  :: Maybe String -> Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)
detail_GET mTokenHeader mServerUrl tmlId =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ addTraceUuidHeader =<< getDocumentTemplateByUuidDto tmlId
