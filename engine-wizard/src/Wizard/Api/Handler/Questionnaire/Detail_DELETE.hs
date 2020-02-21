module Wizard.Api.Handler.Questionnaire.Detail_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_DELETE
   = Header "Authorization" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader qtnUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "QTN_PERM"
      deleteQuestionnaire qtnUuid
      return NoContent
