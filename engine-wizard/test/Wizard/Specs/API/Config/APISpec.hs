module Wizard.Specs.API.Config.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.List_GET

configAPI appContext = with (startWebApp appContext) $ describe "CONFIG API Spec" $ list_get appContext
