module Wizard.Api.Handler.Migration.Questionnaire.List_Current_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.Questionnaire.MigratorService

type List_Current_GET
   = Header "Authorization" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "migrations"
     :> "current"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)

list_current_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)
list_current_GET mTokenHeader qtnUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "QTN_PERM"
      getQuestionnaireMigration qtnUuid
