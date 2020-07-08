module Wizard.Api.Handler.Migration.Questionnaire.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateJM ()
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.Questionnaire.MigratorService

type List_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] MigratorStateCreateDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "migrations"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)

list_POST ::
     Maybe String
  -> MigratorStateCreateDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] MigratorStateDTO)
list_POST mTokenHeader reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< createQuestionnaireMigration qtnUuid reqDto
