module Wizard.Specs.API.ApiKey.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.ApiKey.Detail_DELETE
import Wizard.Specs.API.ApiKey.List_GET
import Wizard.Specs.API.ApiKey.List_POST
import Wizard.Specs.API.Common

apiKeyAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "API KEY API Spec" $ do
    list_GET appContext
    list_POST appContext
    detail_DELETE appContext
