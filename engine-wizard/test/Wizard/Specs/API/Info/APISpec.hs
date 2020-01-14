module Wizard.Specs.API.Info.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Info.List_GET

infoAPI appContext = with (startWebApp appContext) $ describe "INFO API Spec" $ list_get appContext
