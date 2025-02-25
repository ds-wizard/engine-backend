module Wizard.Api.Handler.Questionnaire.Version.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] QuestionnaireVersionChangeDTO
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "versions"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireVersionList)

list_POST
  :: Maybe String
  -> Maybe String
  -> QuestionnaireVersionChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireVersionList)
list_POST mTokenHeader mServerUrl reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createVersion qtnUuid reqDto
