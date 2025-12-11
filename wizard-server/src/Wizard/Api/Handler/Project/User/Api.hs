module Wizard.Api.Handler.Project.User.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Project.User.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type UserAPI =
  Tags "Project User"
    :> List_Suggestions_GET

userApi :: Proxy UserAPI
userApi = Proxy

userServer :: ServerT UserAPI BaseContextM
userServer = list_suggestions_GET
