module Wizard.Api.Handler.Template.File.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.File.TemplateFileService

type Detail_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "templates"
     :> Capture "templateId" String
     :> "files"
     :> Capture "fileUuid" U.UUID
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateFile)

detail_GET ::
     Maybe String
  -> Maybe String
  -> String
  -> U.UUID
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateFile)
detail_GET mTokenHeader mServerUrl tmlId fileUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getTemplateFile fileUuid
