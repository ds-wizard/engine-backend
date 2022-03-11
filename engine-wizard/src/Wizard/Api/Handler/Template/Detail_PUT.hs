module Wizard.Api.Handler.Template.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type Detail_PUT
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] TemplateChangeDTO
     :> "templates"
     :> Capture "templateId" String
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] Template)

detail_PUT ::
     Maybe String
  -> Maybe String
  -> TemplateChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] Template)
detail_PUT mTokenHeader mServerUrl reqDto tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< modifyTemplate tmlId reqDto
