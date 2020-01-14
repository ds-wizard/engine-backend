module Wizard.Specs.API.Template.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.List_GET

templateAPI appContext = with (startWebApp appContext) $ describe "TEMPLATE API Spec" $ list_get appContext
