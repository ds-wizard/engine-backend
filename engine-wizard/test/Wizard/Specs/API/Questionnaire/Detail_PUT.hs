module Wizard.Specs.API.Questionnaire.Detail_PUT
  ( detail_put
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import qualified Shared.Service.Package.PackageMapper as SPM
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /questionnaires/{qtnUuid}
-- ------------------------------------------------------------------------
detail_put :: AppContext -> SpecWith ((), Application)
detail_put appContext =
  describe "PUT /questionnaires/{qtnUuid}" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT qtnUuid = BS.pack $ "/questionnaires/" ++ U.toString qtnUuid

reqHeadersT authHeader = [authHeader, reqCtHeader]

reqDtoT qtn =
  QuestionnaireChangeDTO
    { _questionnaireChangeDTOName = qtn ^. name
    , _questionnaireChangeDTOVisibility = qtn ^. visibility
    , _questionnaireChangeDTOSharing = qtn ^. sharing
    , _questionnaireChangeDTOPermissions = qtn ^. permissions
    , _questionnaireChangeDTOTemplateId = qtn ^. templateId
    , _questionnaireChangeDTOFormatUuid = qtn ^. formatUuid
    }

reqBodyT qtn = encode $ reqDtoT qtn

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext questionnaire1 questionnaire1Edited questionnaire1Ctn []
  create_test_200 "HTTP 200 OK (Owner, VisibleView)" appContext questionnaire2 questionnaire2Edited questionnaire2Ctn []
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    appContext
    questionnaire3
    questionnaire3Edited
    questionnaire3Ctn
    [albertEditPermRecordDto]

create_test_200 title appContext qtn qtnEdited qtnCtn permissions =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ qtn ^. uuid
    let reqHeaders = reqHeadersT reqAuthHeader
    let reqBody = reqBodyT qtnEdited
     -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto =
          toDetailWithPackageWithEventsDTO
            qtnEdited
            qtnCtn
            (SPM.toPackage germanyPackage)
            ["1.0.0"]
            km1WithQ4
            QSDefault
            Nothing
            Nothing
            fReplies
            permissions
            fEventsDto
            qVersionsDto
    let expType (a :: QuestionnaireDetailDTO) = a
     -- AND: Run migrations
    runInContextIO QTN.runMigration appContext
    runInContextIO TML.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    assertResponse expStatus expHeaders expDto expType response ["updatedAt"]
    -- AND: Find a result in DB
    assertExistenceOfQuestionnaireInDB appContext qtnEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT $ questionnaire3 ^. uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext =
  createAuthTest reqMethod (reqUrlT $ questionnaire3 ^. uuid) [reqCtHeader] (reqBodyT questionnaire1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest
    appContext
    reqMethod
    (reqUrlT $ questionnaire3 ^. uuid)
    [reqCtHeader]
    (reqBodyT questionnaire1)
    "QTN_PERM"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    questionnaire1Edited
    "View Questionnaire"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    questionnaire2Edited
    "Administrate Questionnaire"

create_test_403 title appContext qtn qtnEdited reason =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ qtn ^. uuid
    let reqHeaders = reqHeadersT reqNonAdminAuthHeader
    let reqBody = reqBodyT qtnEdited
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
    runInContextIO QTN.runMigration appContext
    runInContextIO TML.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a"
    (reqHeadersT reqAuthHeader)
    (reqBodyT questionnaire1)
    "questionnaire"
    "f08ead5f-746d-411b-aee6-77ea3d24016a"
