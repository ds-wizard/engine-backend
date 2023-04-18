module Wizard.Specs.API.Swagger.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Swagger.List_GET

swaggerAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "SWAGGER API Spec" $ list_get appContext
