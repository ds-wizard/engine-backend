module Wizard.Api.Handler.User.Api where

import Servant

import Wizard.Api.Handler.User.Detail_DELETE
import Wizard.Api.Handler.User.Detail_GET
import Wizard.Api.Handler.User.Detail_PUT
import Wizard.Api.Handler.User.Detail_Password_PUT
import Wizard.Api.Handler.User.Detail_State_PUT
import Wizard.Api.Handler.User.List_Current_GET
import Wizard.Api.Handler.User.List_Current_PUT
import Wizard.Api.Handler.User.List_Current_Password_PUT
import Wizard.Api.Handler.User.List_GET
import Wizard.Api.Handler.User.List_POST
import Wizard.Api.Handler.User.List_Page_GET
import Wizard.Model.Context.BaseContext

type UserAPI
   = List_GET
     :<|> List_Page_GET
     :<|> List_POST
     :<|> List_Current_GET
     :<|> List_Current_PUT
     :<|> List_Current_Password_PUT
     :<|> Detail_GET
     :<|> Detail_PUT
     :<|> Detail_Password_PUT
     :<|> Detail_State_PUT
     :<|> Detail_DELETE

userApi :: Proxy UserAPI
userApi = Proxy

userServer :: ServerT UserAPI BaseContextM
userServer =
  list_GET :<|> list_page_GET :<|> list_POST :<|> list_current_GET :<|> list_current_PUT :<|> list_current_password_PUT :<|>
  detail_GET :<|>
  detail_PUT :<|>
  detail_password_PUT :<|>
  detail_state_PUT :<|>
  detail_DELETE
