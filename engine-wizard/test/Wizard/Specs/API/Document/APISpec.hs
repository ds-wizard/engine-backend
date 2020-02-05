module Wizard.Specs.API.Document.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Document.Detail_DELETE
import Wizard.Specs.API.Document.List_GET
import Wizard.Specs.API.Document.List_POST

documentAPI appContext =
  with (startWebApp appContext) $
  describe "DOCUMENT API Spec" $ do
    list_GET appContext
    list_POST appContext
    detail_DELETE appContext
