module Wizard.Specs.API.UserGroup.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.UserGroup.List_Suggestions_GET

userGroupAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "USER GROUP API Spec" $
      list_suggestions_GET appContext
