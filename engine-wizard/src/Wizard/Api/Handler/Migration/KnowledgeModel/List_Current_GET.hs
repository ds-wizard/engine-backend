module Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.KnowledgeModel.MigratorService

type List_Current_GET
   = Header "Authorization" String
     :> "branches"
     :> Capture "bUuid" String
     :> "migrations"
     :> "current"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)

list_current_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)
list_current_GET mTokenHeader bUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "KM_UPGRADE_PERM"
      getCurrentMigrationDto bUuid
