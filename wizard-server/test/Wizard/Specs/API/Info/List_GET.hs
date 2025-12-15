module Wizard.Specs.API.Info.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Info.InfoDTO
import Shared.Common.Api.Resource.Info.InfoJM ()
import Shared.Common.Database.Migration.Development.Info.Data.Infos
import qualified Shared.Component.Database.Migration.Development.Component.ComponentMigration as CMP_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Constant.ProjectAction
import Wizard.Constant.ProjectImporter
import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.KnowledgeModel.Constant.KnowledgeModel

import SharedTest.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext = describe "GET /wizard-api/" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            infoDTO
              { metamodelVersions =
                  [ InfoMetamodelVersionDTO {name = "Knowledge Model", version = show knowledgeModelMetamodelVersion}
                  , InfoMetamodelVersionDTO {name = "Document Template", version = show documentTemplateMetamodelVersion}
                  , InfoMetamodelVersionDTO {name = "Project Importer", version = show projectImporterMetamodelVersion}
                  , InfoMetamodelVersionDTO {name = "Project Action", version = show projectActionMetamodelVersion}
                  ]
              }
      let expBody = encode expDto
      -- AND: Prepare DB
      runInContextIO CMP_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
