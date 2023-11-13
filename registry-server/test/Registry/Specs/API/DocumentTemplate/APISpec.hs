module Registry.Specs.API.DocumentTemplate.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Specs.API.Common

import Registry.Specs.API.DocumentTemplate.Detail_Bundle_GET
import Registry.Specs.API.DocumentTemplate.Detail_GET
import Registry.Specs.API.DocumentTemplate.List_GET

templateAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "TEMPLATE API Spec" $ do
      list_GET appContext
      detail_GET appContext
      detail_bundle_GET appContext
