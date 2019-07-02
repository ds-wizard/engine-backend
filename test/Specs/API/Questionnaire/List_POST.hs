module Specs.API.Questionnaire.List_POST
  ( list_post
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ

import Api.Resource.Error.ErrorDTO ()
import Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Api.Resource.Questionnaire.QuestionnaireDTO
import Database.DAO.Package.PackageDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import LensesConfig
import Model.Context.AppContext
import Model.Questionnaire.QuestionnaireState
import Service.Questionnaire.QuestionnaireMapper

import Specs.API.Common
import Specs.API.Questionnaire.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /questionnaires/{qtnUuid}
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith Application
list_post appContext =
  describe "POST /questionnaires" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/questionnaires"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto =
  QuestionnaireCreateDTO
  { _questionnaireCreateDTOName = questionnaire1 ^. name
  , _questionnaireCreateDTOPackageId = questionnaire1 ^. packageId
  , _questionnaireCreateDTOAccessibility = questionnaire1 ^. accessibility
  , _questionnaireCreateDTOTagUuids = []
  }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  it "HTTP 201 CREATED" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    let expDto = toSimpleDTO (questionnaire1 & level .~ 1) germanyPackage QSDefault
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO (insertPackage germanyPackage) appContext
    runInContextIO deleteQuestionnaires appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareQuestionnaireCreateDtos resBody expDto
    -- AND: Find a result in DB
    assertExistenceOfQuestionnaireInDB
      appContext
      (((questionnaire1 & level .~ 1) & uuid .~ (resBody ^. uuid)) & replies .~ [])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Common Questionnaire" } |] "packageId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. appConfig) reqMethod reqUrl [] "" "QTN_PERM"
