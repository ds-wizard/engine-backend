module Wizard.Specs.API.Branch.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Branch.Detail_DELETE
import Wizard.Specs.API.Branch.Detail_GET
import Wizard.Specs.API.Branch.Detail_PUT
import Wizard.Specs.API.Branch.List_GET
import Wizard.Specs.API.Branch.List_POST
import Wizard.Specs.API.Common

branchAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "BRANCH API Spec" $ do
      list_GET appContext
      list_POST appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
