module Wizard.Api.Handler.DocumentTemplate.Detail_Pull_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService

type Detail_Pull_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> Capture "documentTemplateId" String
    :> "pull"
    :> Verb POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_pull_POST
  :: Maybe String -> Maybe String -> String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_pull_POST mTokenHeader mServerUrl tmlId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        pullBundleFromRegistry tmlId
        return NoContent
