module Registry.Api.Api where

import Servant

import Registry.Api.Handler.ActionKey.Api
import Registry.Api.Handler.Info.Api
import Registry.Api.Handler.Locale.Api
import Registry.Api.Handler.Organization.Api
import Registry.Api.Handler.Package.Api
import Registry.Api.Handler.Template.Api
import Registry.Model.Context.BaseContext

type ApplicationAPI =
  InfoAPI
    :<|> ActionKeyAPI
    :<|> LocaleAPI
    :<|> OrganizationAPI
    :<|> PackageAPI
    :<|> TemplateAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: ServerT ApplicationAPI BaseContextM
applicationServer = infoServer :<|> actionKeyServer :<|> localeServer :<|> organizationServer :<|> packageServer :<|> templateServer
