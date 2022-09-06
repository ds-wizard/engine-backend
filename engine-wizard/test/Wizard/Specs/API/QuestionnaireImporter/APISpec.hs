module Wizard.Specs.API.QuestionnaireImporter.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.QuestionnaireImporter.Detail_GET
import Wizard.Specs.API.QuestionnaireImporter.Detail_PUT
import Wizard.Specs.API.QuestionnaireImporter.List_GET
import Wizard.Specs.API.QuestionnaireImporter.List_Suggestions_GET

questionnaireImporterAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
  describe "QUESTIONNAIRE IMPORTER API Spec" $ do
    list_GET appContext
    list_suggestions_GET appContext
    detail_GET appContext
    detail_PUT appContext
