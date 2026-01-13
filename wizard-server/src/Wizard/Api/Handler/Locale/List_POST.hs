module Wizard.Api.Handler.Locale.List_POST where

import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Shared.Locale.Api.Resource.Locale.LocaleSimpleJM ()
import Shared.Locale.Model.Locale.LocaleSimple
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Api.Resource.Locale.LocaleCreateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem LocaleCreateDTO
    :> "locales"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleSimple)

list_POST
  :: Maybe String
  -> Maybe String
  -> LocaleCreateDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleSimple)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader
        =<< createLocale reqDto
