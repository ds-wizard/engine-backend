module Wizard.Specs.API.Metric.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Metric.List_GET

metricAPI appContext = with (startWebApp appContext) $ describe "METRIC API Spec" $ list_get appContext
