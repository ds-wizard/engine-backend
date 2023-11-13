module Wizard.Specs.API.DocumentTemplate.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplate.Detail_DELETE
import Wizard.Specs.API.DocumentTemplate.Detail_GET
import Wizard.Specs.API.DocumentTemplate.Detail_PUT
import Wizard.Specs.API.DocumentTemplate.Detail_Pull_POST
import Wizard.Specs.API.DocumentTemplate.List_All_GET
import Wizard.Specs.API.DocumentTemplate.List_DELETE
import Wizard.Specs.API.DocumentTemplate.List_GET
import Wizard.Specs.API.DocumentTemplate.List_Suggestions_GET

documentTemplateAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "DOCUMENT TEMPLATE API Spec" $ do
      list_GET appContext
      list_all_GET appContext
      list_suggestions_GET appContext
      list_DELETE appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
      detail_pull_POST appContext
