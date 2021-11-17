module Wizard.Api.Handler.Document.Detail_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService

type Detail_DELETE
   = Header "Authorization" String
     :> Header "Host" String
     :> "documents"
     :> Capture "docUuid" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_DELETE ::
     Maybe String -> Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader mServerUrl docUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      deleteDocument docUuid
      return NoContent
