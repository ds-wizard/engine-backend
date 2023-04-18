module Wizard.Api.Handler.Questionnaire.Detail_Documents_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Document.DocumentService

type Detail_Documents_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "documents"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page DocumentDTO))

detail_documents_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page DocumentDTO))
detail_documents_GET mTokenHeader mServerUrl qtnUuid mQuery mPage mSize mSort =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getDocumentsForQtn qtnUuid mQuery (Pageable mPage mSize) (parseSortQuery mSort)
