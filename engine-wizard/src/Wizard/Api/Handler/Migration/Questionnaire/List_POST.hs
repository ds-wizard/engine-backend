module Wizard.Api.Handler.Migration.Questionnaire.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateJM ()
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.Questionnaire.MigratorService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] MigratorStateCreateDTO
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "migrations"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] MigratorStateDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> MigratorStateCreateDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] MigratorStateDTO)
list_POST mTokenHeader mServerUrl reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createQuestionnaireMigration qtnUuid reqDto
