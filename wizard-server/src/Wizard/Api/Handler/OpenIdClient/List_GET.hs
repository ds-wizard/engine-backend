module Wizard.Api.Handler.OpenIdClient.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.OpenId.Client.Definition.OpenIdClientDefinitionService
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleJM ()
import WizardLib.Public.Model.OpenId.OpenIdClientSimple

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "open-id-clients"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [OpenIdClientSimple])

list_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [OpenIdClientSimple])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getOpenIdClientDefinitions
