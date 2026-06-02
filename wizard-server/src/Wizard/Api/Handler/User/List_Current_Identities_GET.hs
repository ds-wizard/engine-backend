module Wizard.Api.Handler.User.List_Current_Identities_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.ExternalIdentity.UserExternalIdentityService
import WizardLib.Public.Api.Resource.User.UserOpenIdIdentityDTO
import WizardLib.Public.Api.Resource.User.UserOpenIdIdentityJM ()

type List_Current_Identities_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "current"
    :> "identities"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserOpenIdIdentityDTO])

list_current_identities_GET
  :: Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [UserOpenIdIdentityDTO])
list_current_identities_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getUserIdentities
