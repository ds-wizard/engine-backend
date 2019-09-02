module Specs.API.Organization.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Organization.Current_GET
import Specs.API.Organization.Current_PUT

organizationAPI appContext =
  with (startWebApp appContext) $
  describe "ORGANIZATION API Spec" $ do
    current_get appContext
    current_put appContext
