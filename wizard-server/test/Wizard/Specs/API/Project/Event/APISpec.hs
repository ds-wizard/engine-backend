module Wizard.Specs.API.Project.Event.APISpec where

import Test.Hspec

import Wizard.Specs.API.Project.Event.Detail_GET
import Wizard.Specs.API.Project.Event.List_GET

projectEventAPI appContext =
  describe "PROJECT EVENT API Spec" $ do
    list_GET appContext
    detail_GET appContext
