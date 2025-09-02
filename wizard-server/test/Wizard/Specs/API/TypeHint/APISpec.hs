module Wizard.Specs.API.TypeHint.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.TypeHint.Legacy_POST
import Wizard.Specs.API.TypeHint.List_POST
import Wizard.Specs.API.TypeHint.Test_POST

typeHintAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "TYPEHINT API Spec" $ do
      legacy_POST appContext
      list_POST appContext
      test_POST appContext
