module Wizard.Specs.API.Project.Version.APISpec where

import Test.Hspec

import Wizard.Specs.API.Project.Version.Detail_DELETE
import Wizard.Specs.API.Project.Version.Detail_PUT
import Wizard.Specs.API.Project.Version.List_GET
import Wizard.Specs.API.Project.Version.List_POST

projectVersionAPI appContext =
  describe "PROJECT VERSION API Spec" $ do
    list_GET appContext
    list_POST appContext
    detail_PUT appContext
    detail_DELETE appContext
