module Wizard.Api.Handler.Tenant.List_Suggestions_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.TenantService
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionJM ()
import WizardLib.Public.Model.Tenant.TenantSuggestion

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> "suggestions"
    :> QueryParam "q" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TenantSuggestion])

list_suggestions_GET :: Maybe String -> Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [TenantSuggestion])
list_suggestions_GET mTokenHeader mServerUrl mQuery =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getTenantSuggestions mQuery
