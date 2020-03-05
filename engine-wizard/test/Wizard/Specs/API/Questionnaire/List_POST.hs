module Wizard.Specs.API.Questionnaire.List_POST
  ( list_post
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ

import LensesConfig
import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper

import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /questionnaires
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
      ((((questionnaire1 & level .~ 1) & uuid .~ (resBody ^. uuid)) & replies .~ []) & labels .~ [])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Common Questionnaire" } |] "packageId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createNoPermissionTest (appContext ^. applicationConfig) reqMethod reqUrl [reqCtHeader] reqBody "QTN_PERM"
