module Wizard.Api.Handler.OpenIdClient.Detail_Response_GET where

import Data.Maybe (isJust)
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.OpenId.Client.Flow.OpenIdClientFlowService
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO

type Detail_Response_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> Header "User-Agent" String
    :> "open-id-clients"
    :> Capture "uuid" U.UUID
    :> "response"
    :> QueryParam "clientUrl" String
    :> QueryParam "error" String
    :> QueryParam "code" String
    :> QueryParam "nonce" String
    :> QueryParam "id_token" String
    :> QueryParam "session_state" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

detail_response_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
detail_response_GET mTokenHeader mServerUrl mUserAgent providerUuid mClientUrl mError mCode mNonce mIdToken mSessionState =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< loginUserOrLinkIdentity (isJust mTokenHeader) providerUuid mClientUrl mError mCode mNonce mIdToken mUserAgent mSessionState
