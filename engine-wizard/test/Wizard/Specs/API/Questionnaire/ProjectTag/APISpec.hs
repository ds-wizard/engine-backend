module Wizard.Specs.API.Questionnaire.ProjectTag.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.ProjectTag.List_Suggestions_GET

questionnaireProjectTagAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
  describe "QUESTIONNAIRE PROJECT TAG API Spec" $ do list_suggestions_GET appContext
