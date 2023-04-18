module Registry.Specs.API.Locale.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Specs.API.Common

import Registry.Specs.API.Locale.Detail_Bundle_GET
import Registry.Specs.API.Locale.Detail_GET
import Registry.Specs.API.Locale.List_GET

localeAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "LOCALE API Spec" $ do
      list_get appContext
      detail_get appContext
      detail_bundle_get appContext
