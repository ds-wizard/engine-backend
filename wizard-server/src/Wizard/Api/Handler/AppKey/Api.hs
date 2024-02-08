module Wizard.Api.Handler.AppKey.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.AppKey.Detail_DELETE
import Wizard.Api.Handler.AppKey.List_GET
import Wizard.Api.Handler.AppKey.List_POST
import Wizard.Model.Context.BaseContext

type AppKeyAPI =
  Tags "AppKey"
    :> ( List_GET
          :<|> List_POST
          :<|> Detail_DELETE
       )

appKeyApi :: Proxy AppKeyAPI
appKeyApi = Proxy

appKeyServer :: ServerT AppKeyAPI BaseContextM
appKeyServer = list_GET :<|> list_POST :<|> detail_DELETE
