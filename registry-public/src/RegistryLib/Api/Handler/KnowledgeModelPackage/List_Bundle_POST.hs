module RegistryLib.Api.Handler.KnowledgeModelPackage.List_Bundle_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle

type List_Bundle_POST =
  Header "Authorization" String
    :> ReqBody '[SafeJSON] KnowledgeModelBundle
    :> "knowledge-model-packages"
    :> "bundle"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelBundle)

list_bundle_POST_Api :: Proxy List_Bundle_POST
list_bundle_POST_Api = Proxy
