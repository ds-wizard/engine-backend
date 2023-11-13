module Wizard.Specs.API.Feedback.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Feedback.Detail_GET
import Wizard.Specs.API.Feedback.List_GET
import Wizard.Specs.API.Feedback.List_POST

feedbackAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "FEEDBACK API Spec" $ do
      list_GET appContext
      list_POST appContext
      detail_GET appContext
