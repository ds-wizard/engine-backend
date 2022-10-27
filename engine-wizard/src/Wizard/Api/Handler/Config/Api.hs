module Wizard.Api.Handler.Config.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Config.List_App_GET
import Wizard.Api.Handler.Config.List_App_Logo_DELETE
import Wizard.Api.Handler.Config.List_App_Logo_POST
import Wizard.Api.Handler.Config.List_App_PUT
import Wizard.Api.Handler.Config.List_Bootstrap_GET
import Wizard.Api.Handler.Config.List_Locale_GET
import Wizard.Model.Context.BaseContext

type ConfigAPI
   = Tags "Config"
     :> (List_App_GET
         :<|> List_App_PUT
         :<|> List_App_Logo_POST
         :<|> List_App_Logo_DELETE
         :<|> List_Bootstrap_GET
         :<|> List_Locale_GET)

configApi :: Proxy ConfigAPI
configApi = Proxy

configServer :: ServerT ConfigAPI BaseContextM
configServer =
  list_app_GET :<|> list_app_PUT :<|> list_app_logo_POST :<|> list_app_logo_DELETE :<|> list_bootstrap_GET :<|>
  list_locale_GET
