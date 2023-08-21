module Registry.Specs.API.Config.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Specs.API.Common
import Registry.Specs.API.Config.List_Bootstrap_GET

configAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "CONFIG API Spec" $
      list_bootstrap_GET appContext
