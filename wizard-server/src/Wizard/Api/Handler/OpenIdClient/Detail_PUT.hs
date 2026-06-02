module Wizard.Api.Handler.OpenIdClient.Detail_PUT where

import qualified Data.UUID as U
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

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] OpenIdClientChangeDTO
    :> "open-id-clients"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> OpenIdClientChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] OpenIdClientDetailDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< modifyOpenIdClientDefinition uuid reqDto
