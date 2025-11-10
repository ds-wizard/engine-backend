module Wizard.Api.Handler.KnowledgeModelPackage.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> QueryParam "organizationId" String
    :> QueryParam "kmId" String
    :> QueryParam "q" String
    :> QueryParam "outdated" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelPackageSimpleDTO))

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
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelPackageSimpleDTO))
list_GET mTokenHeader mServerUrl mOrganizationId mKmId mQuery mOutdated mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getPackagesPage mOrganizationId mKmId mQuery mOutdated (Pageable mPage mSize) (parseSortQuery mSort)
