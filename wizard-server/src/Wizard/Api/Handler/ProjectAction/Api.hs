module Wizard.Api.Handler.ProjectAction.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.ProjectAction.Detail_GET
import Wizard.Api.Handler.ProjectAction.Detail_PUT
import Wizard.Api.Handler.ProjectAction.List_GET
import Wizard.Api.Handler.ProjectAction.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type ProjectActionAPI =
  Tags "Project Action"
    :> ( List_GET
          :<|> List_Suggestions_GET
          :<|> Detail_GET
          :<|> Detail_PUT
       )

projectActionApi :: Proxy ProjectActionAPI
projectActionApi = Proxy

projectActionServer :: ServerT ProjectActionAPI BaseContextM
projectActionServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> detail_GET
    :<|> detail_PUT
