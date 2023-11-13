module Wizard.Specs.API.Package.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Package.Detail_DELETE
import Wizard.Specs.API.Package.Detail_GET
import Wizard.Specs.API.Package.Detail_PUT
import Wizard.Specs.API.Package.Detail_Pull_POST
import Wizard.Specs.API.Package.List_DELETE
import Wizard.Specs.API.Package.List_From_Branch_POST
import Wizard.Specs.API.Package.List_GET
import Wizard.Specs.API.Package.List_POST
import Wizard.Specs.API.Package.List_Suggestions_GET

packageAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "PACKAGE API Spec" $ do
      list_GET appContext
      list_suggestions_GET appContext
      list_POST appContext
      list_from_branch_POST appContext
      list_DELETE appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
      detail_pull_POST appContext
