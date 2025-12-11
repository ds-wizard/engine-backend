module Wizard.Api.Handler.Project.Tag.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Project.Tag.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type TagAPI =
  Tags "Project Tag"
    :> List_Suggestions_GET

tagApi :: Proxy TagAPI
tagApi = Proxy

tagServer :: ServerT TagAPI BaseContextM
tagServer = list_suggestions_GET
