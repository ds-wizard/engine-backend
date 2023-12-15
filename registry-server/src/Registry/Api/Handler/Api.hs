module Registry.Api.Handler.Api where

import Servant

import Registry.Api.Handler.ActionKey.Api
import Registry.Api.Handler.Config.Api
import Registry.Api.Handler.DocumentTemplate.Api
import Registry.Api.Handler.Info.Api
import Registry.Api.Handler.Locale.Api
import Registry.Api.Handler.Organization.Api
import Registry.Api.Handler.Package.Api
import Registry.Api.Handler.PersistentCommand.Api
import Registry.Model.Context.BaseContext

type ApplicationAPI =
  InfoAPI
    :<|> ActionKeyAPI
    :<|> ConfigAPI
    :<|> DocumentTemplateAPI
    :<|> LocaleAPI
    :<|> OrganizationAPI
    :<|> PackageAPI
    :<|> PersistentCommandAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: ServerT ApplicationAPI BaseContextM
applicationServer =
  infoServer
    :<|> actionKeyServer
    :<|> configServer
    :<|> documentTemplateServer
    :<|> localeServer
    :<|> organizationServer
    :<|> packageServer
    :<|> persistentCommandServer
