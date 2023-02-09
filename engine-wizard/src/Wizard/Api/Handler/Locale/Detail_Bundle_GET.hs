module Wizard.Api.Handler.Locale.Detail_Bundle_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.LocaleBundle.LocaleBundleService

type Detail_Bundle_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> Capture "lclId" String
    :> "bundle"
    :> Verb GET 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_bundle_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_bundle_GET mTokenHeader mServerUrl lclId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ do
      link <- exportLocaleBundle lclId
      throwError $ FoundError link
