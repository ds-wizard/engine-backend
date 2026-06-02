module Registry.Api.Handler.UserEmailLink.Api where

import Servant

import Registry.Api.Handler.UserEmailLink.List_POST
import Registry.Model.Context.BaseContext

type UserEmailLinkAPI = List_POST

userEmailLinkApi :: Proxy UserEmailLinkAPI
userEmailLinkApi = Proxy

userEmailLinkServer :: ServerT UserEmailLinkAPI BaseContextM
userEmailLinkServer = list_POST
