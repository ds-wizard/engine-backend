module Wizard.Specs.API.Domain.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Domain.Detail_GET

domainAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "DOMAIN API Spec" $ detail_GET appContext
