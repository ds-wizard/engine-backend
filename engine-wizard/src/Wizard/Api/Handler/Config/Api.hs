module Wizard.Api.Handler.Config.Api where

import Servant

import Wizard.Api.Handler.Config.List_Affiliation_GET
import Wizard.Api.Handler.Config.List_Affiliation_PUT
import Wizard.Api.Handler.Config.List_Bootstrap_GET
import Wizard.Api.Handler.Config.List_Client_GET
import Wizard.Api.Handler.Config.List_Client_PUT
import Wizard.Api.Handler.Config.List_Features_GET
import Wizard.Api.Handler.Config.List_Features_PUT
import Wizard.Api.Handler.Config.List_Info_GET
import Wizard.Api.Handler.Config.List_Info_PUT
import Wizard.Api.Handler.Config.List_Organization_GET
import Wizard.Api.Handler.Config.List_Organization_PUT
import Wizard.Model.Context.BaseContext

type ConfigAPI
   = List_Bootstrap_GET
     :<|> List_Affiliation_GET
     :<|> List_Affiliation_PUT
     :<|> List_Client_GET
     :<|> List_Client_PUT
     :<|> List_Features_GET
     :<|> List_Features_PUT
     :<|> List_Info_GET
     :<|> List_Info_PUT
     :<|> List_Organization_GET
     :<|> List_Organization_PUT

configApi :: Proxy ConfigAPI
configApi = Proxy

configServer :: ServerT ConfigAPI BaseContextM
configServer =
  list_bootstrap_GET :<|> list_affiliation_GET :<|> list_affiliation_PUT :<|> list_client_GET :<|> list_client_PUT :<|>
  list_features_GET :<|>
  list_features_PUT :<|>
  list_info_GET :<|>
  list_info_PUT :<|>
  list_organization_GET :<|>
  list_organization_PUT
