module Wizard.Api.Handler.Package.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Shared.Api.Handler.Common
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
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])
list_GET mTokenHeader mOrganizationId mKmId =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "PM_READ_PERM"
      let queryParams = catMaybes [(,) "organizationId" <$> mOrganizationId, (,) "kmId" <$> mKmId]
      getSimplePackagesFiltered queryParams
