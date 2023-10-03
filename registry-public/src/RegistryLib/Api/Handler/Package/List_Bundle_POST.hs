module RegistryLib.Api.Handler.Package.List_Bundle_POST where

import Servant

import Shared.Common.Api.Handler.Common
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleDTO
import WizardLib.KnowledgeModel.Api.Resource.PackageBundle.PackageBundleJM ()

type List_Bundle_POST =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] PackageBundleDTO
    :> "packages"
    :> "bundle"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] PackageBundleDTO)

list_bundle_POST_Api :: Proxy List_Bundle_POST
list_bundle_POST_Api = Proxy
