module Registry.Api.Handler.Package.List_Bundle_POST where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Package.Bundle.PackageBundleService
import Shared.Api.Handler.Common
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Api.Resource.PackageBundle.PackageBundleJM ()
import Shared.Model.Context.TransactionState

type List_Bundle_POST =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] PackageBundleDTO
    :> "packages"
    :> "bundle"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PackageBundleDTO)

list_bundle_POST_Api :: Proxy List_Bundle_POST
list_bundle_POST_Api = Proxy

list_bundle_POST
  :: Maybe String
  -> PackageBundleDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] PackageBundleDTO)
list_bundle_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< importBundle reqDto
