module Wizard.Api.Handler.Template.Detail_Pull_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.TemplateBundle.TemplateBundleService

type Detail_Pull_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> "templates"
     :> Capture "templateId" String
     :> "pull"
     :> Verb POST 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_pull_POST ::
     Maybe String -> Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_pull_POST mTokenHeader mServerUrl tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      pullTemplateBundleFromRegistry tmlId
      return NoContent
