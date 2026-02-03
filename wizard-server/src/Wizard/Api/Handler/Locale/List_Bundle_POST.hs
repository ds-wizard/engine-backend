module Wizard.Api.Handler.Locale.List_Bundle_POST where

import Servant
import Servant.Multipart

import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Common.FileDTO
import Shared.Common.Api.Resource.Common.FileJM ()
import Shared.Common.Model.Context.TransactionState
import Shared.Locale.Api.Resource.Locale.LocaleSimpleJM ()
import Shared.Locale.Model.Locale.LocaleSimple
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.Bundle.LocaleBundleService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem FileDTO
    :> "locales"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleSimple)

list_bundle_POST
  :: Maybe String
  -> Maybe String
  -> FileDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleSimple)
list_bundle_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        importBundle reqDto.content False
