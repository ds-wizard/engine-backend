module Wizard.Api.Handler.Migration.KnowledgeModel.Api where

import Servant

import Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_Conflict_POST
import Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_DELETE
import Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_GET
import Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_POST
import Wizard.Model.Context.BaseContext

type MigrationAPI
   = List_Current_GET
     :<|> List_Current_POST
     :<|> List_Current_DELETE
     :<|> List_Current_Conflict_POST

migrationApi :: Proxy MigrationAPI
migrationApi = Proxy

migrationServer :: ServerT MigrationAPI BaseContextM
migrationServer = list_current_GET :<|> list_current_POST :<|> list_current_DELETE :<|> list_current_conflict_POST
