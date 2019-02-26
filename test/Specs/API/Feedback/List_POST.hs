module Specs.API.Feedback.List_POST
  ( list_post
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ

import Api.Resource.Feedback.FeedbackCreateDTO
import Api.Resource.Feedback.FeedbackDTO
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.Context.AppContext

import Specs.API.Common
import Specs.API.Feedback.Common

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
  , _feedbackCreateDTOPackageId = germanyPackage ^. pId
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
    -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, FeedbackDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareFeedbackDtos resBody reqDto
    -- AND: Compare state in DB with expectation
    assertExistenceOfFeedbackInDB appContext reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { title: "Albert" } |] "questionUuid"
