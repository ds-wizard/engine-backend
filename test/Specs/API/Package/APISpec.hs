module Specs.API.Package.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Package.Detail_DELETE
import Specs.API.Package.Detail_GET
import Specs.API.Package.List_DELETE
import Specs.API.Package.List_GET
import Specs.API.Package.List_Unique_GET

packageAPI appContext =
  with (startWebApp appContext) $
  describe "PACKAGE API Spec" $ do
    list_get appContext
    list_delete appContext
    list_unique_get appContext
    detail_get appContext
    detail_delete appContext
