module Wizard.Api.Handler.Locale.List_Bundle_POST where

import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Resource.Common.FileDTO
import Shared.Api.Resource.Common.FileJM ()
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.Bundle.LocaleBundleService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileDTO
    :> "locales"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDTO)

list_bundle_POST
  :: Maybe String
  -> Maybe String
  -> FileDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleDTO)
list_bundle_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        importBundle reqDto.content False
