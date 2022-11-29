module Wizard.Api.Handler.Locale.List_DELETE where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type List_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> QueryParam "organizationId" String
    :> QueryParam "localeId" String
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mServerUrl mOrganizationId mLocaleId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let queryParams = catMaybes [(,) "organization_id" <$> mOrganizationId, (,) "locale_id" <$> mLocaleId]
        deleteLocalesByQueryParams queryParams
        return NoContent
