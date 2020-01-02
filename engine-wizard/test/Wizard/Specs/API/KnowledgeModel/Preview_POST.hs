module Wizard.Specs.API.KnowledgeModel.Preview_POST
  ( preview_post
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.Migration.Development.Package.Data.Packages
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /knowledge-models/preview
-- ------------------------------------------------------------------------
preview_post :: AppContext -> SpecWith Application
preview_post appContext =
  describe "POST /knowledge-models/preview" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/knowledge-models/preview"

reqHeaders = [reqAuthHeader]

reqDto =
  KnowledgeModelChangeDTO
    { _knowledgeModelChangeDTOPackageId = Just $ germanyPackage ^. pId
    , _knowledgeModelChangeDTOEvents = []
    , _knowledgeModelChangeDTOTagUuids = []
    }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = toKnowledgeModelDTO km1WithQ4
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO PKG.runMigration appContext
    runInContextIO (insertPackage germanyPackage) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. applicationConfig) reqMethod reqUrl [] "" "QTN_PERM"
