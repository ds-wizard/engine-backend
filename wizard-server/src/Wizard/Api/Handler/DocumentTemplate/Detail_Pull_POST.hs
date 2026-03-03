module Wizard.Api.Handler.DocumentTemplate.Detail_Pull_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.Coordinate.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateSimple
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService

type Detail_Pull_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> Capture "id" Coordinate
    :> "pull"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)

detail_pull_POST :: Maybe String -> Maybe String -> Coordinate -> BaseContextM (Headers '[Header "x-trace-uuid" String] DocumentTemplateSimple)
detail_pull_POST mTokenHeader mServerUrl dtId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< pullBundleFromRegistry dtId
