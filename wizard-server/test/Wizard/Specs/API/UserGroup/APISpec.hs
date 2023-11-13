module Wizard.Specs.API.UserGroup.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.UserGroup.Detail_GET
import Wizard.Specs.API.UserGroup.List_Suggestions_GET

userGroupAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "USER GROUP API Spec" $ do
      list_suggestions_GET appContext
      detail_GET appContext
