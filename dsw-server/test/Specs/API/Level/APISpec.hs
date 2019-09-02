module Specs.API.Level.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Level.List_GET

levelAPI appContext = with (startWebApp appContext) $ describe "LEVEL API Spec" $ list_get appContext
