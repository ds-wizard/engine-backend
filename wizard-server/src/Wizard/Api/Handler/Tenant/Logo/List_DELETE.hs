module Wizard.Api.Handler.Tenant.Logo.List_DELETE where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.Config.ConfigLogoService

type List_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> "current"
    :> "logo"
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteLogo
        return NoContent
