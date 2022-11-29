module Registry.Specs.API.Organization.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

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
      list_get appContext
      list_simple_get appContext
      list_post appContext
      detail_get appContext
      detail_put appContext
      detail_delete appContext
      detail_state_put appContext
      detail_token_put appContext
