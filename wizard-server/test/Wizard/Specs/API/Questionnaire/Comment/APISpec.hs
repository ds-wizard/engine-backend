module Wizard.Specs.API.Questionnaire.Comment.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Comment.List_GET

questionnaireCommentAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "QUESTIONNAIRE COMMENT API Spec" $
      list_GET appContext
