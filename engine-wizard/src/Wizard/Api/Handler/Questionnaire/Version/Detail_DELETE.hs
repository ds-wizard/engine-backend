module Wizard.Api.Handler.Questionnaire.Version.Detail_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService

type Detail_DELETE
   = Header "Authorization" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "versions"
     :> Capture "vUuid" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader qtnUuid vUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      deleteVersion qtnUuid vUuid
      return NoContent
