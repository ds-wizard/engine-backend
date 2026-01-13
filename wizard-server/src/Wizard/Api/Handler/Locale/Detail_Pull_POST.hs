module Wizard.Api.Handler.Locale.Detail_Pull_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.Coordinate.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.Locale.Api.Resource.Locale.LocaleSimpleJM ()
import Shared.Locale.Model.Locale.LocaleSimple
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.Bundle.LocaleBundleService

type Detail_Pull_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> Capture "id" Coordinate
    :> "pull"
    :> Verb POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleSimple)

detail_pull_POST :: Maybe String -> Maybe String -> Coordinate -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleSimple)
detail_pull_POST mTokenHeader mServerUrl coordinate =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< pullBundleFromRegistry coordinate
