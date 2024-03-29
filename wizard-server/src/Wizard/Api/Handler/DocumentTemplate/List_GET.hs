module Wizard.Api.Handler.DocumentTemplate.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Model.Context.BaseContext
import Wizard.Service.DocumentTemplate.DocumentTemplateService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "document-templates"
    :> QueryParam "organizationId" String
    :> QueryParam "templateId" String
    :> QueryParam "q" String
    :> QueryParam "outdated" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page DocumentTemplateSimpleDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page DocumentTemplateSimpleDTO))
list_GET mTokenHeader mServerUrl mOrganizationId mTmlId mQuery mOutdated mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getDocumentTemplatesPage mOrganizationId mTmlId mQuery mOutdated (Pageable mPage mSize) (parseSortQuery mSort)
