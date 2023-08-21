module Registry.Api.Handler.Package.List_Bundle_POST where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextLenses ()
import Registry.Service.Package.Bundle.PackageBundleService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleDTO
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleJM ()

list_bundle_POST
  :: Maybe String
  -> PackageBundleDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] PackageBundleDTO)
list_bundle_POST mTokenHeader reqDto =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< importBundle reqDto
