module Specs.API.ActionKey.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.ActionKey.List_POST
import Specs.API.Common

actionKeyAPI appContext = with (startWebApp appContext) $ describe "ACTIONKEY API Spec" $ do list_post appContext
