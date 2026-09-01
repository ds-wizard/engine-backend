module Wizard.Specs.API.Project.Migration.APISpec where

import Test.Hspec

import Wizard.Specs.API.Project.Migration.List_POST

projectMigrationAPI appContext =
  describe "PROJECT MIGRATION API Spec" $
    list_POST appContext
