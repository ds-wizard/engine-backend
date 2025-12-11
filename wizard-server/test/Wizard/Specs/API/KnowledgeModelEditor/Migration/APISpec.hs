module Wizard.Specs.API.KnowledgeModelEditor.Migration.APISpec where

import Test.Hspec

import Wizard.Specs.API.KnowledgeModelEditor.Migration.List_Current_Conflict_All_POST
import Wizard.Specs.API.KnowledgeModelEditor.Migration.List_Current_Conflict_POST
import Wizard.Specs.API.KnowledgeModelEditor.Migration.List_Current_DELETE
import Wizard.Specs.API.KnowledgeModelEditor.Migration.List_Current_GET
import Wizard.Specs.API.KnowledgeModelEditor.Migration.List_Current_POST

knowledgeModelEditorMigrationAPI appContext =
  describe "KNOWLEDGE MODEL EDITOR MIGRATION API Spec" $ do
    list_current_GET appContext
    list_current_POST appContext
    list_current_DELETE appContext
    list_current_conflict_POST appContext
    list_Current_Conflict_All_POST appContext
