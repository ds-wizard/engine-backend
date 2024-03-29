module Registry.Api.Handler.Locale.List_Bundle_POST where

import Servant
import Servant.Multipart

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Service.Locale.Bundle.LocaleBundleService
import RegistryLib.Api.Resource.Locale.LocaleDTO
import RegistryLib.Api.Resource.Locale.LocaleJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Common.FileDTO
import Shared.Common.Api.Resource.Common.FileJM ()
import Shared.Common.Model.Context.TransactionState

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
        importBundle reqDto.content
