module Wizard.Specs.API.Project.Event.APISpec where

import Test.Hspec

import Wizard.Specs.API.Project.Event.Detail_GET
import Wizard.Specs.API.Project.Event.List_GET
import Wizard.Specs.API.Project.Event.List_POST

projectEventAPI appContext =
  describe "PROJECT EVENT API Spec" $ do
    list_GET appContext
    list_POST appContext
    detail_GET appContext
