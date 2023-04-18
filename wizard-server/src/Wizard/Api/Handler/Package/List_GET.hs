module Wizard.Api.Handler.Package.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.PackageService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "packages"
    :> QueryParam "organizationId" String
    :> QueryParam "kmId" String
    :> QueryParam "q" String
    :> QueryParam "state" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page PackageSimpleDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page PackageSimpleDTO))
list_GET mTokenHeader mServerUrl mOrganizationId mKmId mQuery mState mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getPackagesPage mOrganizationId mKmId mQuery mState (Pageable mPage mSize) (parseSortQuery mSort)
