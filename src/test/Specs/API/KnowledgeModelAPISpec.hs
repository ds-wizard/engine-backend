module Specs.API.KnowledgeModelAPISpec where

import Control.Lens
import Data.Aeson
import Data.Aeson (Value(..), object, (.=))
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

import Api.Resources.KnowledgeModel.KnowledgeModelDTO
import Api.Resources.KnowledgeModelContainer.KnowledgeModelContainerDTO
import Database.DAO.Event.EventDAO
import Database.DAO.KnowledgeModel.KnowledgeModelDAO
import qualified
       Database.Migration.KnowledgeModel.KnowledgeModelContainerMigration
       as KMC
import qualified Database.Migration.Package.PackageMigration as PKG
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModelContainer.KnowledgeModelContainer
import Service.Event.EventService
import Service.KnowledgeModel.KnowledgeModelMapper
import Service.KnowledgeModelContainer.KnowledgeModelContainerService

import Fixtures.Event.Events
import Fixtures.KnowledgeModel.AnswersAndFollowUpQuestions
import Fixtures.KnowledgeModel.Chapters
import Fixtures.KnowledgeModel.Experts
import Fixtures.KnowledgeModel.KnowledgeModels
import Fixtures.KnowledgeModel.Questions
import Fixtures.KnowledgeModel.References
import Specs.API.Common

--shouldRespondWith :: HasCallStack => WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldRespondWith r matcher = do
  forM_ (match r matcher) (liftIO . expectationFailure)

knowledgeModelAPI context dspConfig =
  with (startWebApp context dspConfig) $ do
    describe "KNOWLEDGE MODEL API Spec" $
      -- ------------------------------------------------------------------------
      -- GET /users
      -- ------------------------------------------------------------------------
     do
      describe "GET /kmcs/6474b24b-262b-42b1-9451-008e8363f2b6/km" $ do
        let reqMethod = methodGet
        let reqUrl = "/kmcs/6474b24b-262b-42b1-9451-008e8363f2b6/km"
        it "HTTP 200 OK" $
          -- GIVEN: Prepare request
         do
          let reqHeaders = [reqAuthHeader, reqCtHeader]
          liftIO $ PKG.runMigration context dspConfig fakeLogState
          liftIO $ KMC.runMigration context dspConfig fakeLogState
          -- GIVEN: Prepare expectation
          let expStatus = 200
          let expHeaders = [resCtHeader] ++ resCorsHeaders
          let expDto = toKnowledgeModelDTO $ km1
          let expBody = encode expDto
          -- WHEN: Call API
          response <- request reqMethod reqUrl reqHeaders ""
          -- AND: Compare response with expetation
          let responseMatcher =
                ResponseMatcher
                { matchHeaders = expHeaders
                , matchStatus = expStatus
                , matchBody = bodyEquals expBody
                }
          response `shouldRespondWith` responseMatcher
        createAuthTest reqMethod reqUrl [] ""
