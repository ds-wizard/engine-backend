module Wizard.Api.Handler.Questionnaire.Version.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Questionnaire.QuestionnaireVersionList
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "versions"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [QuestionnaireVersionList])

list_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] [QuestionnaireVersionList])
list_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getVersions qtnUuid
