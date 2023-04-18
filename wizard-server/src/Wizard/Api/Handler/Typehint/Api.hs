module Wizard.Api.Handler.Typehint.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Typehint.List_POST
import Wizard.Model.Context.BaseContext

type TypehintAPI =
  Tags "Typehint"
    :> List_POST

typehintApi :: Proxy TypehintAPI
typehintApi = Proxy

typehintServer :: ServerT TypehintAPI BaseContextM
typehintServer = list_POST
