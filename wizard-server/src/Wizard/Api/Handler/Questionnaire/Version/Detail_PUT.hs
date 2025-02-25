module Wizard.Api.Handler.Questionnaire.Version.Detail_PUT where

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

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] QuestionnaireVersionChangeDTO
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "versions"
    :> Capture "vUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireVersionList)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> QuestionnaireVersionChangeDTO
  -> U.UUID
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireVersionList)
detail_PUT mTokenHeader mServerUrl reqDto qtnUuid vUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyVersion qtnUuid vUuid reqDto
