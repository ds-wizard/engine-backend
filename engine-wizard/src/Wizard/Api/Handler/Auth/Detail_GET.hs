module Wizard.Api.Handler.Auth.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Auth.OpenIdService

type Detail_GET
   = Header "Host" String
     :> "auth"
     :> Capture "id" String
     :> QueryParam "flow" String
     :> QueryParam "clientUrl" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_GET ::
     Maybe String
  -> String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_GET mServerUrl authId mFlow mClientUrl =
  runInUnauthService mServerUrl NoTransaction $
  addTraceUuidHeader =<< do
    createAuthenticationUrl authId mFlow mClientUrl
    return NoContent
