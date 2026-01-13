module Wizard.Api.Handler.Locale.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Locale.LocaleChangeDTO
import Wizard.Api.Resource.Locale.LocaleChangeJM ()
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] LocaleChangeDTO
    :> "locales"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> LocaleChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyLocale uuid reqDto
