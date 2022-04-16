module Wizard.Api.Handler.Domain.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Domain.List_GET
import Wizard.Model.Context.BaseContext

type DomainAPI
   = Tags "Domain"
     :> List_GET

domainApi :: Proxy DomainAPI
domainApi = Proxy

domainServer :: ServerT DomainAPI BaseContextM
domainServer = list_GET
