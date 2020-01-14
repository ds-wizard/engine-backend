module Wizard.Specs.API.Token.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Token.List_POST

tokenAPI appContext = with (startWebApp appContext) $ describe "TOKEN API Spec" $ list_post appContext
