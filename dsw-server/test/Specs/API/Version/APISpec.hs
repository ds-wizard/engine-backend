module Specs.API.Version.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Version.Detail_PUT

versionAPI appContext = with (startWebApp appContext) $ describe "VERSION API Spec" $ detail_put appContext
