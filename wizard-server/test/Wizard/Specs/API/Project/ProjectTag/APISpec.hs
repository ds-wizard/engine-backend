module Wizard.Specs.API.Project.ProjectTag.APISpec where

import Test.Hspec

import Wizard.Specs.API.Project.ProjectTag.List_Suggestions_GET

projectTagAPI appContext =
  describe "PROJECT TAG API Spec" $
    do list_suggestions_GET appContext
