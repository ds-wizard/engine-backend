module Wizard.Api.Handler.Token.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Token.Detail_DELETE
import Wizard.Api.Handler.Token.List_Current_DELETE
import Wizard.Api.Handler.Token.List_DELETE
import Wizard.Api.Handler.Token.List_GET
import Wizard.Api.Handler.Token.List_POST
import Wizard.Api.Handler.Token.List_System_POST
import Wizard.Model.Context.BaseContext

type TokenAPI =
  Tags "Token"
    :> ( List_GET
          :<|> List_POST
          :<|> List_DELETE
          :<|> List_System_POST
          :<|> List_Current_DELETE
          :<|> Detail_DELETE
       )

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

tokenServer :: ServerT TokenAPI BaseContextM
tokenServer =
  list_GET
    :<|> list_POST
    :<|> list_DELETE
    :<|> list_system_POST
    :<|> list_current_DELETE
    :<|> detail_DELETE
