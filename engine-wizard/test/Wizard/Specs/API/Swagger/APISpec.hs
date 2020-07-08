module Wizard.Specs.API.Swagger.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Swagger.List_GET

swaggerAPI appContext = with (startWebApp appContext) $ describe "SWAGGER API Spec" $ list_get appContext
