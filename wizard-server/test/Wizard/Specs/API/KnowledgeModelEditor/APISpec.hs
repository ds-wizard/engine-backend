module Wizard.Specs.API.KnowledgeModelEditor.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModelEditor.Detail_DELETE
import Wizard.Specs.API.KnowledgeModelEditor.Detail_GET
import Wizard.Specs.API.KnowledgeModelEditor.Detail_PUT
import Wizard.Specs.API.KnowledgeModelEditor.List_GET
import Wizard.Specs.API.KnowledgeModelEditor.List_POST
import Wizard.Specs.API.KnowledgeModelEditor.Migration.APISpec

knowledgeModelEditorAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "KNOWLEDGE MODEL EDITOR API Spec" $ do
      list_GET appContext
      list_POST appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
      knowledgeModelEditorMigrationAPI appContext
