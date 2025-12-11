module Wizard.Api.Handler.Project.Event.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Project.Event.Detail_GET
import Wizard.Api.Handler.Project.Event.List_GET
import Wizard.Model.Context.BaseContext

type EventAPI =
  Tags "Project Event"
    :> ( List_GET
          :<|> Detail_GET
       )

eventApi :: Proxy EventAPI
eventApi = Proxy

eventServer :: ServerT EventAPI BaseContextM
eventServer = list_GET :<|> detail_GET
