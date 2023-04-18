module Wizard.Api.Handler.DocumentTemplateDraft.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftList
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-template-drafts"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page DocumentTemplateDraftList))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page DocumentTemplateDraftList))
list_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getDraftsPage mQuery (Pageable mPage mSize) (parseSortQuery mSort)
