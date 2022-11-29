module Wizard.Specs.API.Questionnaire.List_POST (
  list_post,
) where

import Data.Aeson (encode)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /questionnaires
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith ((), Application)
list_post appContext =
  describe "POST /questionnaires" $ do
    test_201 appContext
    test_400 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/questionnaires"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT qtn = qtn

reqBodyT qtn = encode (reqDtoT qtn)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 appContext "HTTP 201 CREATED (with token)" False questionnaire1Create [reqAuthHeader]
  create_test_201
    appContext
    "HTTP 201 CREATED (without token)"
    True
    (questionnaire1Create {sharing = AnyoneWithLinkEditQuestionnaire} :: QuestionnaireCreateDTO)
    []

create_test_201 appContext title anonymousSharingEnabled qtn authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT qtn
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto =
            if anonymousSharingEnabled
              then questionnaire1Dto {sharing = AnyoneWithLinkEditQuestionnaire} :: QuestionnaireDTO
              else questionnaire1Dto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      runInContextIO deleteQuestionnaires appContext
      -- AND: Enabled anonymous sharing
      updateAnonymousQuestionnaireSharing appContext anonymousSharingEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareQuestionnaireCreateDtos resBody expDto
      -- AND: Find a result in DB
      (Right eventsInDB) <- runInContextIO (findQuestionnaireEventsById (U.toString $ resBody.uuid)) appContext
      if anonymousSharingEnabled
        then
          assertExistenceOfQuestionnaireInDB
            appContext
            ( questionnaire1
                { uuid = resBody.uuid
                , description = Nothing
                , isTemplate = False
                , sharing = AnyoneWithLinkEditQuestionnaire
                , events = eventsInDB
                , versions = []
                , projectTags = []
                , permissions = []
                , creatorUuid = Nothing
                , answeredQuestions = 0
                , unansweredQuestions = 0
                }
              :: Questionnaire
            )
        else do
          let aPermissions =
                [ (head questionnaire1.permissions)
                    { uuid = (head resBody.permissions).uuid
                    , questionnaireUuid = resBody.uuid
                    }
                  :: QuestionnairePermRecord
                ]
          assertExistenceOfQuestionnaireInDB
            appContext
            ( questionnaire1
                { uuid = resBody.uuid
                , description = Nothing
                , isTemplate = False
                , events = eventsInDB
                , versions = []
                , projectTags = []
                , permissions = aPermissions
                , answeredQuestions = 0
                , unansweredQuestions = 0
                }
              :: Questionnaire
            )

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "packageId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] (reqBodyT questionnaire1Create) "QTN_PERM"
