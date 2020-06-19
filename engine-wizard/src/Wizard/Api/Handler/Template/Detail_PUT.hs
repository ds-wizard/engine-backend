module Wizard.Api.Handler.Template.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateChangeJM ()
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type Detail_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] TemplateChangeDTO
     :> "templates"
     :> Capture "tmlId" String
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] Template)

detail_PUT ::
     Maybe String -> TemplateChangeDTO -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] Template)
detail_PUT mTokenHeader reqDto tmlId =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< modifyTemplate tmlId reqDto
