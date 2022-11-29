module Registry.Specs.API.Template.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Specs.API.Common

import Registry.Specs.API.Template.Detail_Bundle_GET
import Registry.Specs.API.Template.Detail_GET
import Registry.Specs.API.Template.List_GET

templateAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "TEMPLATE API Spec" $ do
      list_get appContext
      detail_get appContext
      detail_bundle_get appContext
