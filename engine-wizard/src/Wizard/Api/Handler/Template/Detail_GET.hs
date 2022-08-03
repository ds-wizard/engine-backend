module Wizard.Api.Handler.Template.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Template.TemplateDetailDTO
import Wizard.Api.Resource.Template.TemplateDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type Detail_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "templates"
     :> Capture "templateId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TemplateDetailDTO)

detail_GET ::
     Maybe String -> Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TemplateDetailDTO)
detail_GET mTokenHeader mServerUrl tmlId =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ addTraceUuidHeader =<< getTemplateByUuidDto tmlId
