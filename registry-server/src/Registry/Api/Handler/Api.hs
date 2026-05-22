module Registry.Api.Handler.Api where

import Servant

import Registry.Api.Handler.Config.Api
import Registry.Api.Handler.DocumentTemplate.Api
import Registry.Api.Handler.Info.Api
import Registry.Api.Handler.KnowledgeModelPackage.Api
import Registry.Api.Handler.Locale.Api
import Registry.Api.Handler.Organization.Api
import Registry.Api.Handler.PersistentCommand.Api
import Registry.Api.Handler.UserEmailLink.Api
import Registry.Model.Context.BaseContext

type ApplicationAPI =
  InfoAPI
    :<|> UserEmailLinkAPI
    :<|> ConfigAPI
    :<|> DocumentTemplateAPI
    :<|> KnowledgeModelPackageAPI
    :<|> LocaleAPI
    :<|> OrganizationAPI
    :<|> PersistentCommandAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: ServerT ApplicationAPI BaseContextM
applicationServer =
  infoServer
    :<|> userEmailLinkServer
    :<|> configServer
    :<|> documentTemplateServer
    :<|> knowledgeModelPackageServer
    :<|> localeServer
    :<|> organizationServer
    :<|> persistentCommandServer
