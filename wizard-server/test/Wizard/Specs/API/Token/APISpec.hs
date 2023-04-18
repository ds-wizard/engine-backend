module Wizard.Specs.API.Token.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Token.Detail_DELETE
import Wizard.Specs.API.Token.List_Current_DELETE
import Wizard.Specs.API.Token.List_GET
import Wizard.Specs.API.Token.List_POST

tokenAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "TOKEN API Spec" $ do
    list_GET appContext
    list_POST appContext
    list_current_DELETE appContext
    detail_DELETE appContext
