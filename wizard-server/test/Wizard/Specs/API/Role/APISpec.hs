module Wizard.Specs.API.Role.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Role.Detail_DELETE
import Wizard.Specs.API.Role.Detail_GET
import Wizard.Specs.API.Role.Detail_PUT
import Wizard.Specs.API.Role.List_GET
import Wizard.Specs.API.Role.List_POST

roleAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "USER ROLE API Spec" $ do
      list_GET appContext
      list_POST appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
