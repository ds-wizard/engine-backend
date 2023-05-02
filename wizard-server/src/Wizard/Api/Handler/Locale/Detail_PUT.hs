module Wizard.Api.Handler.Locale.Detail_PUT where

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
    :> Capture "lclId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> LocaleChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleDTO)
detail_PUT mTokenHeader mServerUrl reqDto lclId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyLocale lclId reqDto
