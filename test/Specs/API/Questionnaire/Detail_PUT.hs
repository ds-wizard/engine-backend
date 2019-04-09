module Specs.API.Questionnaire.Detail_PUT
  ( detail_put
  ) where

import Control.Lens ((^.))
import Data.Aeson (eitherDecode, encode)
import Data.Either (isRight)
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()
import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireDTO
import Api.Resource.Report.ReportJM ()
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import LensesConfig
import Model.Context.AppContext
import Service.Questionnaire.QuestionnaireMapper
import Util.List (elems)

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /questionnaires/{qtnUuid}
-- ------------------------------------------------------------------------
detail_put :: AppContext -> SpecWith Application
detail_put appContext =
  describe "PUT /questionnaires/{qtnUuid}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/questionnaires/af984a75-56e3-49f8-b16f-d6b99599910a"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto =
  QuestionnaireChangeDTO
  { _questionnaireChangeDTOName = questionnaire1Edited ^. name
  , _questionnaireChangeDTOPrivate = questionnaire1Edited ^. private
  , _questionnaireChangeDTOLevel = questionnaire1Edited ^. level
  , _questionnaireChangeDTOReplies = toReplyDTO <$> (questionnaire1Edited ^. replies)
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
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    let expDto = toDetailWithPackageWithEventsDTO questionnaire1Edited netherlandsPackageV2
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO QTN.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (SResponse (Status status _) headers body) = response
    liftIO $ status `shouldBe` expStatus
    liftIO $ (expHeaders `elems` headers) `shouldBe` True
    -- AND: Compare body
    let (Right resBody) = eitherDecode body :: Either String QuestionnaireDTO
    liftIO $ (resBody ^. uuid) `shouldBe` expDto ^. uuid
    liftIO $ (resBody ^. name) `shouldBe` expDto ^. name
    liftIO $ (resBody ^. level) `shouldBe` expDto ^. level
    liftIO $ (resBody ^. package) `shouldBe` expDto ^. package
    liftIO $ (resBody ^. createdAt) `shouldBe` expDto ^. createdAt
    -- AND: Find a result in DB
    eitherQtnFromDb <- runInContextIO (findQuestionnaireById "af984a75-56e3-49f8-b16f-d6b99599910a") appContext
    liftIO $ (isRight eitherQtnFromDb) `shouldBe` True
    let (Right qtnFromDb) = eitherQtnFromDb
    -- AND: Compare state in DB with expectation
    liftIO $ (qtnFromDb ^. name) `shouldBe` (reqDto ^. name)
    liftIO $ (qtnFromDb ^. private) `shouldBe` (reqDto ^. private)
    liftIO $ (qtnFromDb ^. level) `shouldBe` (reqDto ^. level)
    liftIO $ (toReplyDTO <$> (qtnFromDb ^. replies)) `shouldBe` (reqDto ^. replies)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. config) reqMethod reqUrl [] "" "QTN_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest reqMethod "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a" reqHeaders reqBody
