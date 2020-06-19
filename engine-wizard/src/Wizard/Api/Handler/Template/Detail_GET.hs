module Wizard.Api.Handler.Template.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateSimpleDTO
import Wizard.Api.Resource.Template.TemplateSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type Detail_GET
   = Header "Authorization" String
     :> "templates"
     :> Capture "tmlId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateSimpleDTO)

detail_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateSimpleDTO)
detail_GET mTokenHeader tmlId =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getTemplateByUuidDto tmlId
