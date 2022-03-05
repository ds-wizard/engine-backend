module Wizard.Specs.API.Prefab.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Prefab.List_GET

prefabAPI appContext = with (startWebApp appContext) $ describe "PREFAB API Spec" $ list_GET appContext
