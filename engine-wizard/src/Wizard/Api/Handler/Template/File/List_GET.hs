module Wizard.Api.Handler.Template.File.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.File.TemplateFileService

type List_GET
   = Header "Authorization" String
     :> "templates"
     :> Capture "tmlId" String
     :> "files"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [TemplateFile])

list_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [TemplateFile])
list_GET mTokenHeader tmlId =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getTemplateFiles tmlId
