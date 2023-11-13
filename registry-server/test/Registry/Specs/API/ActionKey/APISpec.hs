module Registry.Specs.API.ActionKey.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Specs.API.ActionKey.List_POST
import Registry.Specs.API.Common

actionKeyAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "ACTIONKEY API Spec" $ list_POST appContext
