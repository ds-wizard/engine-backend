module Wizard.Api.Handler.Auth.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Auth.OpenIdService

type Detail_GET
   = "auth"
     :> Capture "id" String
     :> QueryParam "clientUrl" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_GET :: String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_GET authId mClientUrl =
  runInUnauthService $
  addTraceUuidHeader =<< do
    createAuthenticationUrl authId mClientUrl
    return NoContent
