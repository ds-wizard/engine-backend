module RegistryLib.Api.Handler.KnowledgeModelPackage.List_GET where

import Servant

import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryLib.Api.Resource.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Common.Api.Handler.Common

type List_GET =
  Header "Authorization" String
    :> Header "x-user-count" String
    :> Header "x-knowledge-model-package-count" String
    :> Header "x-qtn-count" String
    :> Header "x-knowledge-model-editor-count" String
    :> Header "x-doc-count" String
    :> Header "x-tml-count" String
    :> "knowledge-model-packages"
    :> QueryParam "organizationId" String
    :> QueryParam "kmId" String
    :> QueryParam "metamodelVersion" Int
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageSimpleDTO])

list_GET_Api :: Proxy List_GET
list_GET_Api = Proxy
