module Registry.Api.Api where

import Servant

import Registry.Api.Handler.ActionKey.Api
import Registry.Api.Handler.DocumentTemplate.Api
import Registry.Api.Handler.Info.Api
import Registry.Api.Handler.Locale.Api
import Registry.Api.Handler.Organization.Api
import Registry.Api.Handler.Package.Api
import Registry.Model.Context.BaseContext

type ApplicationAPI =
  InfoAPI
    :<|> ActionKeyAPI
    :<|> DocumentTemplateAPI
    :<|> LocaleAPI
    :<|> OrganizationAPI
    :<|> PackageAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: ServerT ApplicationAPI BaseContextM
applicationServer = infoServer :<|> actionKeyServer :<|> documentTemplateServer :<|> localeServer :<|> organizationServer :<|> packageServer
