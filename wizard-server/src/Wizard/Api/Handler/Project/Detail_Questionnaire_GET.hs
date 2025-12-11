module Wizard.Api.Handler.Project.Detail_Questionnaire_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.ProjectService

type Detail_Questionnaire_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "questionnaire"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectDetailQuestionnaireDTO)

detail_questionnaire_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectDetailQuestionnaireDTO)
detail_questionnaire_GET mTokenHeader mServerUrl uuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectDetailQuestionnaireByUuid uuid
