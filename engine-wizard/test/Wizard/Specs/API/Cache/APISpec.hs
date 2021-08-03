module Wizard.Specs.API.Cache.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Cache.List_DELETE
import Wizard.Specs.API.Common

cacheAPI appContext = with (startWebApp appContext) $ describe "CACHE API Spec" $ list_delete appContext
