module Wizard.Api.Handler.Template.File.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Api.Resource.Template.File.TemplateFileChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.File.TemplateFileService

type List_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] TemplateFileChangeDTO
     :> "templates"
     :> Capture "templateId" String
     :> "files"
     :> PostCreated '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateFile)

list_POST ::
     Maybe String
  -> Maybe String
  -> TemplateFileChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateFile)
list_POST mTokenHeader mServerUrl reqDto tmlId =
  getServiceTokenOrAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< createTemplateFile tmlId reqDto
