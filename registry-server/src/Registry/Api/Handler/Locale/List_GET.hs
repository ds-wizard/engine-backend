module Registry.Api.Handler.Locale.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Locale.LocaleService
import RegistryLib.Api.Resource.Locale.LocaleDTO
import RegistryLib.Api.Resource.Locale.LocaleJM ()
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [LocaleDTO])
list_GET mTokenHeader mOrganizationId mLocaleId mRecommendedAppVersion =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "locale_id" <$> mLocaleId]
        getLocales queryParams mRecommendedAppVersion
