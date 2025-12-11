module Wizard.Specs.API.ProjectAction.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.ProjectAction.Detail_GET
import Wizard.Specs.API.ProjectAction.Detail_PUT
import Wizard.Specs.API.ProjectAction.List_GET
import Wizard.Specs.API.ProjectAction.List_Suggestions_GET

projectActionAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "PROJECT ACTION API Spec" $ do
      list_GET appContext
      list_suggestions_GET appContext
      detail_GET appContext
      detail_PUT appContext
