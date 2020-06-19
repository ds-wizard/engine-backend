module Wizard.Api.Handler.Template.Detail_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Template.TemplateService

type Detail_DELETE
   = Header "Authorization" String
     :> "templates"
     :> Capture "tmlId" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader tmlId =
  getServiceTokenOrAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      deleteTemplate tmlId
      return NoContent
