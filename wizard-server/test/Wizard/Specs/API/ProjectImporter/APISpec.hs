module Wizard.Specs.API.ProjectImporter.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.ProjectImporter.Detail_GET
import Wizard.Specs.API.ProjectImporter.Detail_PUT
import Wizard.Specs.API.ProjectImporter.List_GET
import Wizard.Specs.API.ProjectImporter.List_Suggestions_GET

projectImporterAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "PROJECT IMPORTER API Spec" $ do
      list_GET appContext
      list_suggestions_GET appContext
      detail_GET appContext
      detail_PUT appContext
