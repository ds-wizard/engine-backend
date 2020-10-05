module Wizard.Api.Handler.Questionnaire.Detail_Documents_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService

type Detail_Documents_GET
   = Header "Authorization" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "documents"
     :> QueryParam "q" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page DocumentDTO))

detail_documents_GET ::
     Maybe String
  -> String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page DocumentDTO))
detail_documents_GET mTokenHeader qtnUuid mQuery mPage mSize mSort =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< getDocumentsForQuestionnaire qtnUuid mQuery (Pageable mPage mSize) (parseSortQuery mSort)
