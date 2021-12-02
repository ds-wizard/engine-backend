module Wizard.Api.Handler.Token.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Token.List_POST
import Wizard.Model.Context.BaseContext

type TokenAPI
   = Tags "Token"
     :> List_POST

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

tokenServer :: ServerT TokenAPI BaseContextM
tokenServer = list_POST
