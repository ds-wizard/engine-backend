module Wizard.Api.Handler.Locale.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Locale.LocaleDTO
import Wizard.Api.Resource.Locale.LocaleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> QueryParam "organizationId" String
    :> QueryParam "mLocaleId" String
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page LocaleDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page LocaleDTO))
list_GET mTokenHeader mServerUrl mOrganizationId mLocaleId mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getLocalesPage mOrganizationId mLocaleId mQuery (Pageable mPage mSize) (parseSortQuery mSort)
