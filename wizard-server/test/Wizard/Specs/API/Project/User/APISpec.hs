module Wizard.Specs.API.Project.User.APISpec where

import Test.Hspec

import Wizard.Specs.API.Project.User.List_Suggestions_GET

projectUserAPI appContext =
  describe "PROJECT USER API Spec" $
    list_suggestions_GET appContext
