module Wizard.Specs.API.ExternalLink.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.ExternalLink.List_GET

externalLinkAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "EXTERNAL LINK API Spec" $ do
      list_GET appContext
