module Wizard.Api.Handler.Auth.Detail_Callback_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.OpenId.Client.Flow.OpenIdClientFlowService
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO

type Detail_Callback_GET =
  Header "Host" String
    :> Header "User-Agent" String
    :> "auth"
    :> Capture "id" String
    :> "callback"
    :> QueryParam "clientUrl" String
    :> QueryParam "error" String
    :> QueryParam "code" String
    :> QueryParam "nonce" String
    :> QueryParam "id_token" String
    :> QueryParam "session_state" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

detail_callback_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
detail_callback_GET mServerUrl mUserAgent authId mClientUrl mError mCode mNonce mIdToken mSessionState =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< loginUser authId mClientUrl mError mCode mNonce mIdToken mUserAgent mSessionState
