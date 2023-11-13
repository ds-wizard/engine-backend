module Registry.Specs.API.Organization.APISpec where

import Test.Hspec
import Test.Hspec.Wai

import Registry.Specs.API.Common
import Registry.Specs.API.Organization.Detail_DELETE
import Registry.Specs.API.Organization.Detail_GET
import Registry.Specs.API.Organization.Detail_PUT
import Registry.Specs.API.Organization.Detail_State_PUT
import Registry.Specs.API.Organization.Detail_Token_PUT
import Registry.Specs.API.Organization.List_GET
import Registry.Specs.API.Organization.List_POST
import Registry.Specs.API.Organization.List_Simple_GET

organizationAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "ORGANIZATION API Spec" $ do
      list_GET appContext
      list_simple_GET appContext
      list_POST appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
      detail_state_PUT appContext
      detail_token_PUT appContext
