module Registry.Api.Handler.DocumentTemplate.Detail_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.DocumentTemplate.DocumentTemplateService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

type Detail_GET =
  "document-templates"
    :> Capture "documentTemplateId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)

detail_GET :: String -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateDetailDTO)
detail_GET tmlId = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getDocumentTemplateById tmlId
