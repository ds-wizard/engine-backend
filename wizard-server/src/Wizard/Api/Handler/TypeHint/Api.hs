module Wizard.Api.Handler.TypeHint.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.TypeHint.Legacy_POST
import Wizard.Api.Handler.TypeHint.List_POST
import Wizard.Api.Handler.TypeHint.Test_POST
import Wizard.Model.Context.BaseContext

type TypeHintAPI =
  Tags "TypeHint"
    :> (Legacy_POST :<|> List_POST :<|> Test_POST)

typeHintApi :: Proxy TypeHintAPI
typeHintApi = Proxy

typeHintServer :: ServerT TypeHintAPI BaseContextM
typeHintServer = legacy_POST :<|> list_POST :<|> test_POST
