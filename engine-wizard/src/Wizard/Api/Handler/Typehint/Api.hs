module Wizard.Api.Handler.Typehint.Api where

import Servant

import Wizard.Api.Handler.Typehint.List_POST
import Wizard.Model.Context.BaseContext

type TypehintAPI = List_POST

typehintApi :: Proxy TypehintAPI
typehintApi = Proxy

typehintServer :: ServerT TypehintAPI BaseContextM
typehintServer = list_POST
