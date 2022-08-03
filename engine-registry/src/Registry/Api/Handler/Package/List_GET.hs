module Registry.Api.Handler.Package.List_GET where

import Data.Maybe (catMaybes)
import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Api.Resource.Package.PackageSimpleJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Package.PackageService
import Shared.Api.Handler.Common
import Shared.Constant.Api
import Shared.Model.Context.TransactionState

type List_GET
   = Header "Authorization" String
     :> Header "x-user-count" String
     :> Header "x-pkg-count" String
     :> Header "x-qtn-count" String
     :> Header "x-branch-count" String
     :> Header "x-doc-count" String
     :> Header "x-tml-count" String
     :> "packages"
     :> QueryParam "organizationId" String
     :> QueryParam "kmId" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy

list_GET ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [PackageSimpleDTO])
list_GET mTokenHeader xUserCountHeaderValue xPkgCountHeaderValue xQtnCountHeaderValue xBranchCountHeaderValue xDocCountHeaderValue xTmlCountHeaderValue organizationId kmId =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService Transactional $
    addTraceUuidHeader =<< do
      let queryParams = catMaybes [(,) "organization_id" <$> organizationId, (,) "km_id" <$> kmId]
      let headers =
            catMaybes
              [ (,) xUserCountHeaderName <$> xUserCountHeaderValue
              , (,) xPkgCountHeaderName <$> xPkgCountHeaderValue
              , (,) xQtnCountHeaderName <$> xQtnCountHeaderValue
              , (,) xBranchCountHeaderName <$> xBranchCountHeaderValue
              , (,) xDocCountHeaderName <$> xDocCountHeaderValue
              , (,) xTmlCountHeaderName <$> xTmlCountHeaderValue
              ]
      getSimplePackagesFiltered queryParams headers
