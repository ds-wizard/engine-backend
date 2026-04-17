module Wizard.Api.Handler.OpenIdClient.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.OpenId.Client.Definition.OpenIdClientDefinitionService
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM ()
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] OpenIdClientChangeDTO
    :> "open-id-clients"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> OpenIdClientChangeDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< createOpenIdClientDefinition reqDto
