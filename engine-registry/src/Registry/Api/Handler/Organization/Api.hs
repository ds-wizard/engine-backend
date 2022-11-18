module Registry.Api.Handler.Organization.Api where

import Servant

import Registry.Api.Handler.Organization.Detail_DELETE
import Registry.Api.Handler.Organization.Detail_GET
import Registry.Api.Handler.Organization.Detail_PUT
import Registry.Api.Handler.Organization.Detail_State_PUT
import Registry.Api.Handler.Organization.Detail_Token_PUT
import Registry.Api.Handler.Organization.List_GET
import Registry.Api.Handler.Organization.List_POST
import Registry.Api.Handler.Organization.List_Simple_GET
import Registry.Model.Context.BaseContext

type OrganizationAPI =
  List_GET
    :<|> List_Simple_GET
    :<|> List_POST
    :<|> Detail_GET
    :<|> Detail_PUT
    :<|> Detail_DELETE
    :<|> Detail_State_PUT
    :<|> Detail_Token_PUT

organizationApi :: Proxy OrganizationAPI
organizationApi = Proxy

organizationServer :: ServerT OrganizationAPI BaseContextM
organizationServer =
  list_GET
    :<|> list_simple_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> detail_state_PUT
    :<|> detail_token_PUT
