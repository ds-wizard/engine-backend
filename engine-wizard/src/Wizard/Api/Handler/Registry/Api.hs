module Wizard.Api.Handler.Registry.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Registry.List_Confirmation_POST
import Wizard.Api.Handler.Registry.List_Signup_POST
import Wizard.Model.Context.BaseContext

type RegistryAPI
   = Tags "Registry"
     :> (List_Signup_POST
         :<|> List_Confirmation_POST)

registryApi :: Proxy RegistryAPI
registryApi = Proxy

registryServer :: ServerT RegistryAPI BaseContextM
registryServer = list_signup_POST :<|> list_confirmation_POST
