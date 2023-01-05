module Wizard.Api.Handler.Locale.List_POST where

import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Locale.LocaleCreateDTO
import Wizard.Api.Resource.Locale.LocaleCreateJM ()
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem LocaleCreateDTO
    :> "locales"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> LocaleCreateDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader
        =<< createLocale reqDto
