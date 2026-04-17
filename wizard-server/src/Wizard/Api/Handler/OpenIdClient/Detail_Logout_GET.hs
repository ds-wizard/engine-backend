module Wizard.Api.Handler.OpenIdClient.Detail_Logout_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.Login.LoginService

type Detail_Logout_GET =
  Header "Host" String
    :> "open-id-clients"
    :> Capture "uuid" U.UUID
    :> "logout"
    :> QueryParam "sid" String
    :> Verb GET 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_logout_GET
  :: Maybe String -> U.UUID -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_logout_GET mServerUrl _providerUuid mSid =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      deleteLoginTokenBySessionState mSid
      return NoContent
