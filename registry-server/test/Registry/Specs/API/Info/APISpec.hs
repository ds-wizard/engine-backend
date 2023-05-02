module Registry.Specs.API.Info.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Specs.API.Common
import Registry.Specs.API.Info.List_GET

infoAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "INFO API Spec" $ list_get appContext
