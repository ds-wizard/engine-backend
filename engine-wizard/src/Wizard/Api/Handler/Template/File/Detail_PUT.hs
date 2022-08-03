module Wizard.Api.Handler.Template.File.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.Template.Template
import Shared.Model.Template.TemplateJM ()
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.File.TemplateFileChangeDTO
import Wizard.Api.Resource.Template.File.TemplateFileChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.File.TemplateFileService

type Detail_PUT
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] TemplateFileChangeDTO
     :> "templates"
     :> Capture "templateId" String
     :> "files"
     :> Capture "fileUuid" String
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateFile)

detail_PUT ::
     Maybe String
  -> Maybe String
  -> TemplateFileChangeDTO
  -> String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateFile)
detail_PUT mTokenHeader mServerUrl reqDto tmlId fileUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyTemplateFile fileUuid reqDto
