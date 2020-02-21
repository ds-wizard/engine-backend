module Wizard.Api.Handler.Document.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService

type List_GET
   = Header "Authorization" String
     :> "documents"
     :> QueryParam "questionnaireUuid" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [DocumentDTO])

list_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [DocumentDTO])
list_GET mTokenHeader mQuestionnaireUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "DMP_PERM"
      let queryParams = catMaybes [(,) "questionnaireUuid" <$> mQuestionnaireUuid]
      getDocumentsFiltered queryParams
