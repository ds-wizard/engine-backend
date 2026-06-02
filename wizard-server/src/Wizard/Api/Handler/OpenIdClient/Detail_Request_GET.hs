module Wizard.Api.Handler.OpenIdClient.Detail_Request_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.OpenId.Client.Flow.OpenIdClientFlowService

type Detail_Request_GET =
  Header "Host" String
    :> "open-id-clients"
    :> Capture "uuid" U.UUID
    :> "request"
    :> QueryParam "flow" String
    :> QueryParam "clientUrl" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_request_GET
  :: Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_request_GET mServerUrl providerUuid mFlow mClientUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< do
      createAuthenticationUrl providerUuid mFlow mClientUrl
      return NoContent
