module Wizard.Api.Handler.Dev.Operation.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Dev.Operation.List_Executions_POST
import Wizard.Api.Handler.Dev.Operation.List_GET
import Wizard.Model.Context.BaseContext

type DevOperationAPI
   = Tags "DevOperations"
     :> (List_GET
         :<|> List_Executions_POST)

devOperationApi :: Proxy DevOperationAPI
devOperationApi = Proxy

devOperationServer :: ServerT DevOperationAPI BaseContextM
devOperationServer = list_GET :<|> list_executions_POST
