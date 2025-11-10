module Registry.Specs.API.KnowledgeModelPackage.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Specs.API.Common
import Registry.Specs.API.KnowledgeModelPackage.Detail_Bundle_GET
import Registry.Specs.API.KnowledgeModelPackage.Detail_GET
import Registry.Specs.API.KnowledgeModelPackage.List_GET

knowledgeModelPackageAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "KNOWLEDGE MODEL PACKAGE API Spec" $ do
      list_GET appContext
      detail_GET appContext
      detail_bundle_GET appContext
