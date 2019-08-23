module Specs.API.Config.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Config.List_GET

configAPI appContext = with (startWebApp appContext) $ describe "CONFIG API Spec" $ list_get appContext
