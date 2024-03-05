module Wizard.Specs.API.Questionnaire.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnairePerm
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/questionnaires
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/questionnaires" $ do
    test_201 appContext
    test_400 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/questionnaires"

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
      (Right eventsInDB) <- runInContextIO (findQuestionnaireEventsByUuid resBody.uuid) appContext
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
                }
              :: Questionnaire
            )
        else do
          let aPermissions =
                [ (head questionnaire1.permissions)
                    { questionnaireUuid = resBody.uuid
                    }
                  :: QuestionnairePerm
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
