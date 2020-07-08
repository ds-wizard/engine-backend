module Wizard.Api.Handler.Template.File.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.File.TemplateFileService

type Detail_GET
   = Header "Authorization" String
     :> "templates"
     :> Capture "templateId" String
     :> "files"
     :> Capture "fileUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateFile)

detail_GET :: Maybe String -> String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateFile)
detail_GET mTokenHeader tmlId fileUuid =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getTemplateFile tmlId fileUuid
