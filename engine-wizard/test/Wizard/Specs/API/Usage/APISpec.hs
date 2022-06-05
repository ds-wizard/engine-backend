module Wizard.Specs.API.Usage.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Usage.List_Current_GET

usageAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "USAGE API Spec" $ list_current_GET appContext
