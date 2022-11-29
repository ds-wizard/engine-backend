module Registry.Api.Handler.Template.Detail_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Template.TemplateDetailDTO
import Registry.Api.Resource.Template.TemplateDetailJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Template.TemplateService
import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState

type Detail_GET =
  "templates"
    :> Capture "tmlId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemplateDetailDTO)

detail_GET :: String -> BaseContextM (Headers '[Header "x-trace-uuid" String] TemplateDetailDTO)
detail_GET tmlId = runInUnauthService NoTransaction $ addTraceUuidHeader =<< getTemplateById tmlId
