module Wizard.Api.Handler.Admin.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Admin.List_Operations_Executions_POST
import Wizard.Api.Handler.Admin.List_Operations_GET
import Wizard.Model.Context.BaseContext

type AdminAPI
   = Tags "Admin"
     :> (List_Operations_GET
         :<|> List_Operations_Executions_POST)

adminApi :: Proxy AdminAPI
adminApi = Proxy

adminServer :: ServerT AdminAPI BaseContextM
adminServer = list_operations_GET :<|> list_operations_executions_POST
