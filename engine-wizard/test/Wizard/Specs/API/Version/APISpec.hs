module Wizard.Specs.API.Version.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Version.Detail_PUT

versionAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "VERSION API Spec" $ detail_put appContext
