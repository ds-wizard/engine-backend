module Wizard.Specs.API.Migration.KnowledgeModel.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Migration.KnowledgeModel.List_Current_Conflict_POST
import Wizard.Specs.API.Migration.KnowledgeModel.List_Current_DELETE
import Wizard.Specs.API.Migration.KnowledgeModel.List_Current_GET
import Wizard.Specs.API.Migration.KnowledgeModel.List_Current_POST

migrationAPI appContext =
  with (startWebApp appContext) $
  describe "MIGRATION KNOWLEDGE MODEL API Spec" $ do
    list_current_GET appContext
    list_current_POST appContext
    list_current_DELETE appContext
    list_current_conflict_POST appContext
