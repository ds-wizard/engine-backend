module Wizard.Specs.API.Submission.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Submission.List_POST

submissionAPI appContext = with (startWebApp appContext) $ describe "SUBMISSION API Spec" $ list_post appContext
