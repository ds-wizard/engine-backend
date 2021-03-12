module Wizard.Api.Handler.Questionnaire.Version.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService

type List_GET
   = Header "Authorization" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "versions"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [QuestionnaireVersionDTO])

list_GET :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [QuestionnaireVersionDTO])
list_GET mTokenHeader qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getVersions qtnUuid
