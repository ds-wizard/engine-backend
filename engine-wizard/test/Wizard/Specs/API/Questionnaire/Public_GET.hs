module Wizard.Specs.API.Questionnaire.Public_GET
  ( public_get
  ) where

import Data.Aeson (encode)
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.DAO.PublicPackage.PublicPackageDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Wizard.Database.Migration.Development.PublicPackage.Data.PublicPackages
import qualified Wizard.Database.Migration.Development.PublicPackage.PublicPackageMigration as PUBQTN
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper
import Wizard.Service.Package.PackageMapper

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires/public
-- ------------------------------------------------------------------------
public_get :: AppContext -> SpecWith Application
public_get appContext =
  describe "GET /questionnaires/public" $ do
    test_200 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/questionnaires/public"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    let expDto =
          QuestionnaireDetailDTO
            { _questionnaireDetailDTOUuid = fromJust . U.fromString $ "a870d5c7-0e0a-4110-95ae-932cb65c6a6a"
            , _questionnaireDetailDTOName = "Public Questionnaire"
            , _questionnaireDetailDTOLevel = 2
            , _questionnaireDetailDTOAccessibility = PublicQuestionnaire
            , _questionnaireDetailDTOState = QSDefault
            , _questionnaireDetailDTOPackage = toSimpleDTO . toPackage $ publicPackage
            , _questionnaireDetailDTOSelectedTagUuids = []
            , _questionnaireDetailDTOKnowledgeModel = toKnowledgeModelDTO km1WithQ4
            , _questionnaireDetailDTOReplies = []
            , _questionnaireDetailDTOLabels = []
            , _questionnaireDetailDTOOwnerUuid = Nothing
            , _questionnaireDetailDTOCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
            , _questionnaireDetailDTOUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
            }
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO PUBQTN.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireDetailDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareQuestionnaireDtos resBody expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext =
  it "HTTP 400 BAD REQUEST - Public questionnaire is not set up" $
      -- GIVEN: Prepare expectation
   do
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createUserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Public Questionnaire"
    let expBody = encode expDto
    -- AND: Delete public questionnaire
    runInContextIO deletePublicPackages appContext
    -- WHEN: Call APIA
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
