module Wizard.Api.Handler.Template.File.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Api.Resource.Template.File.TemplateFileChangeJM ()
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.File.TemplateFileService

type List_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] TemplateFileChangeDTO
     :> "templates"
     :> Capture "tmlId" String
     :> "files"
     :> PostCreated '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateFile)

list_POST ::
     Maybe String
  -> TemplateFileChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateFile)
list_POST mTokenHeader reqDto tmlId =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< createTemplateFile tmlId reqDto
