module Wizard.Api.Handler.Auth.Api where

import Servant

import Wizard.Api.Handler.Auth.Detail_Callback_GET
import Wizard.Api.Handler.Auth.Detail_GET
import Wizard.Model.Context.BaseContext

type AuthAPI
   = Detail_GET
     :<|> Detail_Callback_GET

authApi :: Proxy AuthAPI
authApi = Proxy

authServer :: ServerT AuthAPI BaseContextM
authServer = detail_GET :<|> detail_callback_GET