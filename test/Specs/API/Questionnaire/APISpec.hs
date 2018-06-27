module Specs.API.Questionnaire.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Questionnaire.Public_GET

questionnaireAPI appContext = with (startWebApp appContext) $ describe "QUESTIONNAIRE API Spec" $ public_get appContext
