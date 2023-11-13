module Wizard.Specs.API.Config.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.List_Bootstrap_GET
import Wizard.Specs.API.Config.List_Locale_GET

configAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "CONFIG API Spec" $ do
      list_bootstrap_GET appContext
      list_locale_GET appContext
