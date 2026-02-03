module Wizard.Api.Handler.Locale.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Locale.LocaleDetailDTO
import Wizard.Api.Resource.Locale.LocaleDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDetailDTO)

detail_GET :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleDetailDTO)
detail_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ addTraceUuidHeader =<< getLocaleByUuid uuid
