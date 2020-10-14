module Wizard.Api.Handler.Package.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Package.PackageSimpleJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Package.PackageService

type List_GET
   = Header "Authorization" String
     :> "packages"
     :> QueryParam "organizationId" String
     :> QueryParam "kmId" String
     :> QueryParam "q" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page PackageSimpleDTO))

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page PackageSimpleDTO))
list_GET mTokenHeader mOrganizationId mKmId mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< getPackagesPage mOrganizationId mKmId mQuery (Pageable mPage mSize) (parseSortQuery mSort)
