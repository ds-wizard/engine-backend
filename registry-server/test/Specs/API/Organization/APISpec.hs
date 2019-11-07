module Specs.API.Organization.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Organization.Detail_DELETE
import Specs.API.Organization.Detail_GET
import Specs.API.Organization.Detail_PUT
import Specs.API.Organization.Detail_State_PUT
import Specs.API.Organization.Detail_Token_PUT
import Specs.API.Organization.List_GET
import Specs.API.Organization.List_POST

organizationAPI appContext =
  with (startWebApp appContext) $
  describe "ORGANIZATION API Spec" $ do
    list_get appContext
    list_post appContext
    detail_get appContext
    detail_put appContext
    detail_delete appContext
    detail_state_put appContext
    detail_token_put appContext
