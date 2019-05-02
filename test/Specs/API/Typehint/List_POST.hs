module Specs.API.Typehint.List_POST
  ( list_post
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Typehint.TypehintDTO
import Api.Resource.Typehint.TypehintRequestDTO
import Database.DAO.Package.PackageDAO
import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Database.Migration.Development.Package.Data.Packages
import qualified
       Database.Migration.Development.Package.PackageMigration as PKG
import LensesConfig
import Model.Context.AppContext

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /typehints
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith Application
list_post appContext =
  describe "POST /typehints" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/typehints"

reqHeaders = [reqAuthHeader]

reqDto =
  TypehintRequestDTO
  { _typehintRequestDTOPackageId = Just $ germanyPackage ^. pId
  , _typehintRequestDTOEvents = []
  , _typehintRequestDTOQuestionUuid = q4_it1_q6_aYes_followUpQuestion5 ^. uuid
  , _typehintRequestDTOQ = "dog"
  }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto =
          [ TypehintDTO
            { _typehintDTOIntId = "op-p000001"
            , _typehintDTOName = "Life Science Ontology"
            , _typehintDTOUrl = "https://example.com/ontologies/${id}"
            }
          , TypehintDTO
            { _typehintDTOIntId = "op-p000008"
            , _typehintDTOName = "Mathematical Ontology"
            , _typehintDTOUrl = "https://example.com/ontologies/${id}"
            }
          , TypehintDTO
            { _typehintDTOIntId = "op-p000015"
            , _typehintDTOName = "Legal Ontology"
            , _typehintDTOUrl = "https://example.com/ontologies/${id}"
            }
          ]
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
test_403 appContext = createNoPermissionTest (appContext ^. appConfig) reqMethod reqUrl [] "" "QTN_PERM"
