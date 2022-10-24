module Wizard.Api.Handler.Auth.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Auth.Detail_Callback_GET
import Wizard.Api.Handler.Auth.Detail_GET
import Wizard.Api.Handler.Auth.Detail_Logout_GET
import Wizard.Model.Context.BaseContext

type AuthAPI
   = Tags "Auth"
     :> (Detail_GET
         :<|> Detail_Callback_GET
         :<|> Detail_Logout_GET)

authApi :: Proxy AuthAPI
authApi = Proxy

authServer :: ServerT AuthAPI BaseContextM
authServer = detail_GET :<|> detail_callback_GET :<|> detail_logout_GET
