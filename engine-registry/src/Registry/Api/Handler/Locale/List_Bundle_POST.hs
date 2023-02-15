module Registry.Api.Handler.Locale.List_Bundle_POST where

import Servant
import Servant.Multipart

import Registry.Api.Handler.Common
import Registry.Api.Resource.Locale.LocaleDTO
import Registry.Api.Resource.Locale.LocaleJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.LocaleBundle.LocaleBundleService
import Shared.Api.Handler.Common
import Shared.Api.Resource.Common.FileDTO
import Shared.Api.Resource.Common.FileJM ()
import Shared.Model.Context.TransactionState

type List_Bundle_POST =
  Header "Authorization" String
    :> MultipartForm Mem FileDTO
    :> "locales"
    :> "bundle"
    :> PostCreated '[SafeJSON] (Headers '[Header "x-trace-uuid" String] LocaleDTO)

list_bundle_POST
  :: Maybe String
  -> FileDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] LocaleDTO)
list_bundle_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        importLocaleBundle reqDto.content
