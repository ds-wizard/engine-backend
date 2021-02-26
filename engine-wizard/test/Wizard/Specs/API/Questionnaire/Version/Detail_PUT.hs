module Wizard.Specs.API.Questionnaire.Version.Detail_PUT
  ( detail_PUT
  ) where

import Control.Lens ((&), (.~))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.API.Questionnaire.Version.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /questionnaires/{qtnUuid}/versions/{vUuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /questionnaires/{qtnUuid}/versions/{vUuid}" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/questionnaires/af984a75-56e3-49f8-b16f-d6b99599910a/versions/bd6611c8-ea11-48ab-adaa-3ce51b66aae5"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = questionnaireVersion1EditedChangeDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 20O OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = questionnaireVersion1EditedDto
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO QTN.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireVersionDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareQuestionnaireVersionCreateDtos resBody expDto
    -- AND: Find a result in DB
    assertExistenceOfQuestionnaireInDB appContext (questionnaire1 & versions .~ [questionnaireVersion1Edited])

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/questionnaires/00084a75-56e3-49f8-b16f-d6b99599910a/versions/bd6611c8-ea11-48ab-adaa-3ce51b66aae5"
    reqHeaders
    reqBody
    "questionnaire"
    "00084a75-56e3-49f8-b16f-d6b99599910a"
