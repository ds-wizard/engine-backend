module Registry.Api.Handler.Template.List_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Service.Template.TemplateService
import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()

type List_GET
   = Header "Authorization" String
     :> "templates"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [Template])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy

list_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [Template])
list_GET mTokenHeader =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService $ addTraceUuidHeader =<< getTemplates
