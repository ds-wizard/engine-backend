module Wizard.Specs.API.App.Plan.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.App.Plan.Detail_DELETE
import Wizard.Specs.API.App.Plan.Detail_PUT
import Wizard.Specs.API.App.Plan.List_POST
import Wizard.Specs.API.Common

appPlanAPI appContext =
  with (startWebApp appContext) $
  describe "APP PLAN API Spec" $ do
    list_POST appContext
    detail_PUT appContext
    detail_DELETE appContext
