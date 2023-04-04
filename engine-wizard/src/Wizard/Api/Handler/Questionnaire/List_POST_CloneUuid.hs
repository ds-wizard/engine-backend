module Wizard.Api.Handler.Questionnaire.List_POST_CloneUuid where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type List_POST_CloneUuid =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "clone"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireDTO)

list_POST_CloneUuid
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireDTO)
list_POST_CloneUuid mTokenHeader mServerUrl cloneUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< cloneQuestionnaire cloneUuid
