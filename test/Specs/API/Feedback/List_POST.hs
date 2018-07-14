module Specs.API.Feedback.List_POST
  ( list_post
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Data.Either (isRight)
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ

import Api.Resource.Feedback.FeedbackCreateDTO
import Database.DAO.Feedback.FeedbackDAO
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.Context.AppContext

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /feedbacks
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith Application
list_post appContext =
  describe "POST /feedbacks" $ do
    test_200 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/feedbacks"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto =
  FeedbackCreateDTO
  { _feedbackCreateDTOQuestionUuid = question1 ^. uuid
  , _feedbackCreateDTOPackageId = elixirCzPackage2Dto ^. pId
  , _feedbackCreateDTOTitle = "New feedback"
  , _feedbackCreateDTOContent = "Some new feedback description"
  }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Find a result
    eitheFeedbacks <- runInContextIO findFeedbacks appContext
    liftIO $ (isRight eitheFeedbacks) `shouldBe` True
    let (Right feedbacks) = eitheFeedbacks
    -- AND: Compare response with expectation
    let (SResponse (Status status _) headers body) = response
    liftIO $ status `shouldBe` expStatus
    liftIO $ headers `shouldBe` expHeaders
    -- AND: Compare state in DB with expectation
    liftIO $ (length feedbacks) `shouldBe` 1
    let feedback = feedbacks !! 0
    liftIO $ (feedback ^. questionUuid) `shouldBe` (reqDto ^. questionUuid)
    liftIO $ (feedback ^. packageId) `shouldBe` (reqDto ^. packageId)
    liftIO $ (feedback ^. title) `shouldBe` (reqDto ^. title)
    liftIO $ (feedback ^. content) `shouldBe` (reqDto ^. content)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { title: "Albert" } |] "questionUuid"
