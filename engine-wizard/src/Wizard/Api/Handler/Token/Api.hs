module Wizard.Api.Handler.Token.Api where

import Servant

import Wizard.Api.Handler.Token.List_POST
import Wizard.Model.Context.BaseContext

type TokenAPI = List_POST

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

tokenServer :: ServerT TokenAPI BaseContextM
tokenServer = list_POST
