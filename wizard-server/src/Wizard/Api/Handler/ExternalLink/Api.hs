module Wizard.Api.Handler.ExternalLink.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.ExternalLink.List_GET
import Wizard.Model.Context.BaseContext

type ExternalLinkAPI =
  Tags "ExternalLink"
    :> List_GET

externalLinkApi :: Proxy ExternalLinkAPI
externalLinkApi = Proxy

externalLinkServer :: ServerT ExternalLinkAPI BaseContextM
externalLinkServer = list_GET
