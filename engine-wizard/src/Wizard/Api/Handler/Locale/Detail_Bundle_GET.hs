module Wizard.Api.Handler.Locale.Detail_Bundle_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.TemporaryFile.TemporaryFileDTO
import Wizard.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.LocaleBundle.LocaleBundleService

type Detail_Bundle_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> Capture "lclId" String
    :> "bundle"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_bundle_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_bundle_GET mTokenHeader mServerUrl lclId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getTemporaryFileWithLocaleBundle lclId
