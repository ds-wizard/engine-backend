module Wizard.Api.Handler.Document.List_GET where

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

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "documents"
    :> QueryParam "questionnaireUuid" U.UUID
    :> QueryParam "documentTemplateId" String
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page DocumentDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page DocumentDTO))
list_GET mTokenHeader mServerUrl mQuestionnaireUuid mDocumentTemplateId mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getDocumentsPageDto mQuestionnaireUuid mDocumentTemplateId mQuery (Pageable mPage mSize) (parseSortQuery mSort)
