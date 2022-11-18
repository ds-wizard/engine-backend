module Wizard.Api.Handler.App.Plan.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.App.Plan.Detail_DELETE
import Wizard.Api.Handler.App.Plan.Detail_PUT
import Wizard.Api.Handler.App.Plan.List_Current_GET
import Wizard.Api.Handler.App.Plan.List_POST
import Wizard.Model.Context.BaseContext

type PlanAPI =
  Tags "Plan"
    :> ( List_Current_GET
          :<|> List_POST
          :<|> Detail_PUT
          :<|> Detail_DELETE
       )

planApi :: Proxy PlanAPI
planApi = Proxy

planServer :: ServerT PlanAPI BaseContextM
planServer = list_current_GET :<|> list_POST :<|> detail_PUT :<|> detail_DELETE
