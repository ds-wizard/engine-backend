module Wizard.Specs.API.Info.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Info.List_GET

infoAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "INFO API Spec" $ list_GET appContext
