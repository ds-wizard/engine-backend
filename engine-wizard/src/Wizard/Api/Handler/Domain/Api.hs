module Wizard.Api.Handler.Domain.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Domain.Detail_GET
import Wizard.Model.Context.BaseContext

type DomainAPI
   = Tags "Domain"
     :> Detail_GET

domainApi :: Proxy DomainAPI
domainApi = Proxy

domainServer :: ServerT DomainAPI BaseContextM
domainServer = detail_GET
