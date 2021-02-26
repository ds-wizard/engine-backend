module Wizard.Specs.API.Questionnaire.Version.List_POST
  ( list_POST
  ) where

import Control.Lens ((&), (.~), (^.))
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
-- POST /questionnaires/{qtnUuid}/versions
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /questionnaires/{qtnUuid}/versions" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/questionnaires/af984a75-56e3-49f8-b16f-d6b99599910a/versions"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = questionnaireVersion1ChangeDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 201 CREATED" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = questionnaireVersion1Dto
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
    let updatedVersions = [questionnaireVersion1, questionnaireVersion1 & uuid .~ (resBody ^. uuid)]
    assertExistenceOfQuestionnaireInDB appContext (questionnaire1 & versions .~ updatedVersions)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
