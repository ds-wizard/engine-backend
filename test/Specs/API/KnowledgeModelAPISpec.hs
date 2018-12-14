module Specs.API.KnowledgeModelAPISpec where

import Control.Lens
import Data.Aeson
import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import qualified
       Database.Migration.Development.Branch.BranchMigration as B
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import qualified
       Database.Migration.Development.Package.PackageMigration as PKG
import LensesConfig
import Service.KnowledgeModel.KnowledgeModelMapper

import Specs.API.Common
import Specs.Common

knowledgeModelAPI appContext =
  with (startWebApp appContext) $ do
    let dswConfig = appContext ^. config
    describe "KNOWLEDGE MODEL API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /users
      -- ------------------------------------------------------------------------
     do
      describe "GET /branches/6474b24b-262b-42b1-9451-008e8363f2b6/km" $
          -- GIVEN: Prepare request
       do
        let reqMethod = methodGet
        let reqUrl = "/branches/6474b24b-262b-42b1-9451-008e8363f2b6/km"
        let reqHeaders = [reqAuthHeader, reqCtHeader]
        let reqBody = ""
        it "HTTP 200 OK" $ do
          runInContextIO PKG.runMigration appContext
          runInContextIO B.runMigration appContext
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = toKnowledgeModelDTO $ km1
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expectation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0/km" reqHeaders reqBody
