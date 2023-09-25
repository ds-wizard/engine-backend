module Wizard.Specs.API.Questionnaire.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.QuestionnaireMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as SPM

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/questionnaires/{qtnUuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/questionnaires/{qtnUuid}" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT qtnUuid = BS.pack $ "/wizard-api/questionnaires/" ++ U.toString qtnUuid

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT qtn =
  QuestionnaireChangeDTO
    { name = qtn.name
    , description = qtn.description
    , visibility = qtn.visibility
    , sharing = qtn.sharing
    , projectTags = qtn.projectTags
    , permissions = qtn.permissions
    , documentTemplateId = qtn.documentTemplateId
    , formatUuid = qtn.formatUuid
    , isTemplate = qtn.isTemplate
    }

reqBodyT qtn = encode $ reqDtoT qtn

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    appContext
    questionnaire1
    questionnaire1Edited
    questionnaire1Ctn
    []
    True
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Owner, VisibleView)"
    appContext
    questionnaire2
    questionnaire2Edited
    questionnaire2Ctn
    []
    False
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Non-Owner, Private, Sharing, Anonymous Enabled)"
    appContext
    questionnaire10
    questionnaire10Edited
    questionnaire10Ctn
    [qtn10NikolaEditPermRecordDto]
    False
    [reqNonAdminAuthHeader]
    True

create_test_200 title appContext qtn qtnEdited qtnCtn permissions showComments authHeader anonymousEnabled =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ qtn.uuid
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT qtnEdited
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto =
            toDetailWithPackageWithEventsDTO
              qtnEdited
              qtnCtn
              (SPM.toPackage germanyPackage)
              km1WithQ4
              QSDefault
              (Just wizardDocumentTemplate)
              (Just formatJson)
              fReplies
              ( if showComments
                  then qtnThreadsDto
                  else M.empty
              )
              permissions
              qVersionsDto
              Nothing
      let expType (a :: QuestionnaireDetailDTO) = a
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
      runInContextIO (insertQuestionnaire questionnaire10) appContext
      -- AND: Enabled anonymous sharing
      updateAnonymousQuestionnaireSharing appContext anonymousEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]
      -- AND: Find a result in DB
      assertExistenceOfQuestionnaireInDB appContext qtnEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT questionnaire3.uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext =
  createAuthTest reqMethod (reqUrlT questionnaire3.uuid) [reqCtHeader] (reqBodyT questionnaire1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest
    appContext
    reqMethod
    (reqUrlT questionnaire3.uuid)
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
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private, Sharing, Anonymous Disabled)"
    appContext
    questionnaire10
    questionnaire10Edited
    "Administrate Questionnaire"

create_test_403 title appContext qtn qtnEdited reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ qtn.uuid
      let reqHeaders = reqHeadersT [reqNonAdminAuthHeader]
      let reqBody = reqBodyT qtnEdited
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
      runInContextIO (insertQuestionnaire questionnaire10) appContext
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
  createNotFoundTest'
    reqMethod
    "/wizard-api/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a"
    (reqHeadersT [reqAuthHeader])
    (reqBodyT questionnaire1)
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
