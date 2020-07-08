module Registry.Api.Api where

import Servant

import Registry.Api.Handler.ActionKey.Api
import Registry.Api.Handler.Info.Api
import Registry.Api.Handler.Organization.Api
import Registry.Api.Handler.Package.Api
import Registry.Api.Handler.Template.Api
import Registry.Model.Context.BaseContext

type AppAPI
   = InfoAPI
     :<|> ActionKeyAPI
     :<|> OrganizationAPI
     :<|> PackageAPI
     :<|> TemplateAPI

appApi :: Proxy AppAPI
appApi = Proxy

appServer :: ServerT AppAPI BaseContextM
appServer = infoServer :<|> actionKeyServer :<|> organizationServer :<|> packageServer :<|> templateServer
