module Wizard.Api.Handler.Role.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Role.Detail_DELETE
import Wizard.Api.Handler.Role.Detail_GET
import Wizard.Api.Handler.Role.Detail_PUT
import Wizard.Api.Handler.Role.List_GET
import Wizard.Api.Handler.Role.List_POST
import Wizard.Model.Context.BaseContext

type RoleAPI =
  Tags "Role"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
       )

roleApi :: Proxy RoleAPI
roleApi = Proxy

roleServer :: ServerT RoleAPI BaseContextM
roleServer =
  list_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
