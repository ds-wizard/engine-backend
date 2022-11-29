module Wizard.Specs.API.Template.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Template.Detail_DELETE
import Wizard.Specs.API.Template.Detail_GET
import Wizard.Specs.API.Template.Detail_PUT
import Wizard.Specs.API.Template.Detail_Pull_POST
import Wizard.Specs.API.Template.List_All_GET
import Wizard.Specs.API.Template.List_DELETE
import Wizard.Specs.API.Template.List_GET
import Wizard.Specs.API.Template.List_POST
import Wizard.Specs.API.Template.List_Suggestions_GET

templateAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "TEMPLATE API Spec" $ do
      list_GET appContext
      list_all_get appContext
      list_suggestions_GET appContext
      list_post appContext
      list_delete appContext
      detail_get appContext
      detail_put appContext
      detail_delete appContext
      detail_pull_post appContext
