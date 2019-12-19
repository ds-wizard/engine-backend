module Wizard.Specs.API.Typehint.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Typehint.List_POST

typehintAPI appContext = with (startWebApp appContext) $ describe "TYPEHINT API Spec" $ list_post appContext
