module Wizard.Api.Handler.DocumentTemplate.Detail_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.DocumentTemplateService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] DocumentTemplateChangeDTO
    :> "document-templates"
    :> Capture "documentTemplateId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> DocumentTemplateChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)
detail_PUT mTokenHeader mServerUrl reqDto tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyDocumentTemplate tmlId reqDto
