module Wizard.Specs.API.OpenIdClient.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common

import Wizard.Specs.API.OpenIdClient.Detail_DELETE
import Wizard.Specs.API.OpenIdClient.Detail_GET
import Wizard.Specs.API.OpenIdClient.Detail_PUT
import Wizard.Specs.API.OpenIdClient.List_GET
import Wizard.Specs.API.OpenIdClient.List_POST

openIdClientAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "OPEN ID CLIENT API Spec" $ do
      list_GET appContext
      list_POST appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
