module Specs.API.Feedback.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Feedback.Detail_GET
import Specs.API.Feedback.List_GET
import Specs.API.Feedback.List_POST

feedbackAPI appContext =
  with (startWebApp appContext) $
  describe "FEEDBACK API Spec" $ do
    list_get appContext
    list_post appContext
    detail_get appContext
