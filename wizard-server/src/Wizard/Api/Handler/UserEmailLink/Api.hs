module Wizard.Api.Handler.UserEmailLink.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.UserEmailLink.List_POST
import Wizard.Model.Context.BaseContext

type UserEmailLinkAPI =
  Tags "User Email Link"
    :> List_POST

userEmailLinkApi :: Proxy UserEmailLinkAPI
userEmailLinkApi = Proxy

userEmailLinkServer :: ServerT UserEmailLinkAPI BaseContextM
userEmailLinkServer = list_POST
