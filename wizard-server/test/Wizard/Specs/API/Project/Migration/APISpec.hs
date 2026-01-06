module Wizard.Specs.API.Project.Migration.APISpec where

import Test.Hspec

import Wizard.Specs.API.Project.Migration.List_Current_Completion_POST
import Wizard.Specs.API.Project.Migration.List_Current_DELETE
import Wizard.Specs.API.Project.Migration.List_Current_GET
import Wizard.Specs.API.Project.Migration.List_Current_PUT
import Wizard.Specs.API.Project.Migration.List_POST

projectMigrationAPI appContext =
  describe "PROJECT MIGRATION API Spec" $ do
    list_POST appContext
    list_current_GET appContext
    list_current_PUT appContext
    list_current_DELETE appContext
    list_current_completion_POST appContext
