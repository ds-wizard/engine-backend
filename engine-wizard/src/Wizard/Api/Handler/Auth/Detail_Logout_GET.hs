module Wizard.Api.Handler.Auth.Detail_Logout_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.UserToken.Login.LoginService

type Detail_Logout_GET =
  Header "Host" String
    :> "auth"
    :> Capture "id" String
    :> "logout"
    :> QueryParam "sid" String
    :> Verb GET 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_logout_GET
  :: Maybe String -> String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_logout_GET mServerUrl authId mSid =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      deleteLoginTokenBySessionState mSid
      return NoContent
