module Wizard.Api.Handler.Questionnaire.Version.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService

type List_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "versions"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [QuestionnaireVersionDTO])

list_GET ::
     Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [QuestionnaireVersionDTO])
list_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getVersions qtnUuid
