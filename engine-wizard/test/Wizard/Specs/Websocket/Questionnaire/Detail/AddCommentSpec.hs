module Wizard.Specs.Websocket.Questionnaire.Detail.AddCommentSpec where

import Control.Lens ((^.))
import Data.Aeson
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U

import Wizard.Specs.Common
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.Questionnaire.Detail.Common

addCommentSpec appContext = describe "addComment" $ test200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 appContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
   do
    let qtn = questionnaire10
    runInContext U.runMigration appContext
    runInContextIO TML_Migration.runMigration appContext
    runInContextIO (insertPackage germanyPackage) appContext
    runInContextIO (insertQuestionnaire questionnaire10) appContext
    runInContextIO (insertQuestionnaire questionnaire7) appContext
    -- AND: Connect to websocket
    ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers appContext (questionnaire10 ^. uuid)
    ((c4, s4), (c5, s5), (c6, s6)) <- connectTestWebsocketUsers appContext (questionnaire7 ^. uuid)
    -- WHEN:
    write_AddComment c1 acche_rQ2_t1_1'
    -- THEN:
    read_AddComment c1 ace_rQ2_t1_1'
    read_AddComment c2 ace_rQ2_t1_1'
    read_AddComment c3 ace_rQ2_t1_1'
    nothingWasReceived c4
    nothingWasReceived c5
    nothingWasReceived c6
    -- AND: Close sockets
    closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_AddComment connection replyDto = do
  let reqDto = SetContent_ClientQuestionnaireActionDTO replyDto
  sendMessage connection reqDto

read_AddComment connection expReplyDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerQuestionnaireActionDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerQuestionnaireActionDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
