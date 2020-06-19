module Wizard.Api.Handler.Template.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateChangeDTO
import Wizard.Api.Resource.Template.TemplateChangeJM ()
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type List_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] TemplateChangeDTO
     :> "templates"
     :> PostCreated '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] Template)

list_POST :: Maybe String -> TemplateChangeDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] Template)
list_POST mTokenHeader reqDto =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< createTemplate reqDto
