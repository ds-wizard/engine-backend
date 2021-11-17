module Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.KnowledgeModel.MigratorService

type List_Current_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] MigratorStateCreateDTO
     :> "branches"
     :> Capture "bUuid" String
     :> "migrations"
     :> "current"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)

list_current_POST ::
     Maybe String
  -> Maybe String
  -> MigratorStateCreateDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)
list_current_POST mTokenHeader mServerUrl reqDto bUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< createMigration bUuid reqDto
