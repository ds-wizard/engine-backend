module Registry.Api.Handler.KnowledgeModelPackage.Api where

import Servant

import Registry.Api.Handler.KnowledgeModelPackage.Detail_Bundle_GET
import Registry.Api.Handler.KnowledgeModelPackage.Detail_GET
import Registry.Api.Handler.KnowledgeModelPackage.List_Bundle_POST
import Registry.Api.Handler.KnowledgeModelPackage.List_GET
import Registry.Model.Context.BaseContext
import RegistryLib.Api.Handler.KnowledgeModelPackage.List_Bundle_POST
import RegistryLib.Api.Handler.KnowledgeModelPackage.List_GET

type KnowledgeModelPackageAPI =
  List_GET
    :<|> List_Bundle_POST
    :<|> Detail_GET
    :<|> Detail_Bundle_GET

knowledgeModelPackageApi :: Proxy KnowledgeModelPackageAPI
knowledgeModelPackageApi = Proxy

knowledgeModelPackageServer :: ServerT KnowledgeModelPackageAPI BaseContextM
knowledgeModelPackageServer = list_GET :<|> list_bundle_POST :<|> detail_GET :<|> detail_bundle_GET
