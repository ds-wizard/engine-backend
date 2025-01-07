module Wizard.Specs.API.DocumentTemplateDraft.Folder.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplateDraft.Folder.List_Delete_POST
import Wizard.Specs.API.DocumentTemplateDraft.Folder.List_Move_POST

documentTemplateDraftFolderAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "DOCUMENT TEMPLATE DRAFT FOLDER API Spec" $ do
      list_delete_POST appContext
      list_move_POST appContext
