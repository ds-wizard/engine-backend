module Wizard.Api.Handler.Organization.Api where

import Servant

import Wizard.Api.Handler.Organization.List_Current_GET
import Wizard.Api.Handler.Organization.List_Current_PUT
import Wizard.Model.Context.BaseContext

type OrganizationAPI
   = List_Current_GET
     :<|> List_Current_PUT

organizationApi :: Proxy OrganizationAPI
organizationApi = Proxy

organizationServer :: ServerT OrganizationAPI BaseContextM
organizationServer = list_current_GET :<|> list_current_PUT
