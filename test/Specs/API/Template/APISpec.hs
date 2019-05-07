module Specs.API.Template.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Template.List_GET

templateAPI appContext = with (startWebApp appContext) $ describe "TEMPLATE API Spec" $ list_get appContext
