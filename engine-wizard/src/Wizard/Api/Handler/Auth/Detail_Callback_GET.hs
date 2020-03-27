module Wizard.Api.Handler.Auth.Detail_Callback_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Model.Context.BaseContext
import Wizard.Service.Auth.OpenIdService

type Detail_Callback_GET
   = "auth"
     :> Capture "id" String
     :> "callback"
     :> QueryParam "error" String
     :> QueryParam "code" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] TokenDTO)

detail_callback_GET ::
     String -> Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] TokenDTO)
detail_callback_GET authId mError mCode = runInUnauthService $ addTraceUuidHeader =<< loginUser authId mError mCode