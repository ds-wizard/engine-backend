module Specs.API.KnowledgeModelAPISpec where

import Control.Lens
import Data.Aeson
import Data.Aeson (Value(..), (.=), object)
import Data.ByteString.Lazy
import Data.Foldable
import Data.Maybe
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import qualified Test.Hspec.Expectations.Pretty as TP
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher
import qualified Web.Scotty as S

import Api.Resource.Branch.BranchDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import qualified Database.Migration.Branch.BranchMigration as KMC
import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions
import Database.Migration.Branch.Data.KnowledgeModel.Chapters
import Database.Migration.Branch.Data.KnowledgeModel.Experts
import Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
import Database.Migration.Branch.Data.KnowledgeModel.Questions
import Database.Migration.Branch.Data.KnowledgeModel.References
import qualified Database.Migration.Package.PackageMigration as PKG
import Model.Branch.Branch
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Service.Branch.BranchService
import Service.Event.EventService
import Service.KnowledgeModel.KnowledgeModelMapper

import Specs.API.Common
import Specs.Common

knowledgeModelAPI context dswConfig =
  with (startWebApp context dswConfig) $ do
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
          liftIO $ PKG.runMigration context dswConfig fakeLogState
          liftIO $ KMC.runMigration context dswConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = toKnowledgeModelDTO $ km1
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders reqBody
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] reqBody
        createNoPermissionTest dswConfig reqMethod reqUrl [] reqBody "KM_PERM"
        createNotFoundTest reqMethod "/branches/dc9fe65f-748b-47ec-b30c-d255bbac64a0/km" reqHeaders reqBody
