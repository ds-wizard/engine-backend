module Registry.Api.Handler.Template.Detail_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Service.Template.TemplateService
import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()

type Detail_GET
   = "templates"
     :> Capture "tmlId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] Template)

detail_GET :: String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] Template)
detail_GET tmlId = runInUnauthService $ addTraceUuidHeader =<< getTemplateById tmlId
