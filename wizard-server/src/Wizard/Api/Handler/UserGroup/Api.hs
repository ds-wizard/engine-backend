module Wizard.Api.Handler.UserGroup.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.UserGroup.List_Suggestions_GET
import Wizard.Model.Context.BaseContext

type UserGroupAPI =
  Tags "User Group"
    :> List_Suggestions_GET

userGroupApi :: Proxy UserGroupAPI
userGroupApi = Proxy

userGroupServer :: ServerT UserGroupAPI BaseContextM
userGroupServer = list_suggestions_GET
