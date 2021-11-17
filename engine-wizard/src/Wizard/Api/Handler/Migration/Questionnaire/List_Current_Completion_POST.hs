module Wizard.Api.Handler.Migration.Questionnaire.List_Current_Completion_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.Questionnaire.MigratorService

type List_Current_Completion_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "migrations"
     :> "current"
     :> "completion"
     :> Verb POST 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_current_completion_POST ::
     Maybe String -> Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_current_completion_POST mTokenHeader mServerUrl qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      finishQuestionnaireMigration qtnUuid
      return NoContent
