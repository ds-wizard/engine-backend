module Registry.Api.Handler.Package.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Api.Resource.Package.PackageSimpleJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Package.PackageService
import Shared.Constant.Api

type List_GET
   = Header "Authorization" String
     :> Header "x-user-count" String
     :> Header "x-pkg-count" String
     :> Header "x-qtn-count" String
     :> "packages"
     :> QueryParam "organizationId" String
     :> QueryParam "kmId" String
     :> Get '[ JSON] (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])
list_GET mTokenHeader xUserCountHeaderValue xPkgCountHeaderValue xQtnCountHeaderValue organizationId kmId =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService $
    addTraceUuidHeader =<< do
      let queryParams = catMaybes [(,) "organizationId" <$> organizationId, (,) "kmId" <$> kmId]
      let headers =
            catMaybes
              [ (,) xUserCountHeaderName <$> xUserCountHeaderValue
              , (,) xPkgCountHeaderName <$> xPkgCountHeaderValue
              , (,) xQtnCountHeaderName <$> xQtnCountHeaderValue
              ]
      getSimplePackagesFiltered queryParams headers
