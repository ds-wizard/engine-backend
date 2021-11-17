module Wizard.Api.Handler.Migration.Questionnaire.List_Current_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeJM ()
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.Questionnaire.MigratorService

type List_Current_PUT
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] MigratorStateChangeDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "migrations"
     :> "current"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)

list_current_PUT ::
     Maybe String
  -> Maybe String
  -> MigratorStateChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)
list_current_PUT mTokenHeader mServerUrl reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< modifyQuestionnaireMigration qtnUuid reqDto
