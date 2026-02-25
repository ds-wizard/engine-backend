module Wizard.Api.Handler.TypeHint.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.TypeHint.List_POST
import Wizard.Api.Handler.TypeHint.Test_POST
import Wizard.Model.Context.BaseContext

type TypeHintAPI =
  Tags "TypeHint"
    :> ( List_POST
          :<|> Test_POST
       )

typeHintApi :: Proxy TypeHintAPI
typeHintApi = Proxy

typeHintServer :: ServerT TypeHintAPI BaseContextM
typeHintServer =
  list_POST
    :<|> test_POST
