module Wizard.Specs.Websocket.Project.Detail.DeleteCommentThreadSpec where

import Data.Aeson
import Data.Foldable (traverse_)
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.Websocket.ProjectMessageDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.DAO.Project.ProjectDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.Projects
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Project.Project

import Wizard.Specs.Common
import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.Project.Detail.Common

deleteCommentThreadSpec appContext = describe "deleteCommentThread" $ test200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 appContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
    do
      runInContext U.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      runInContextIO (insertProject project10) appContext
      runInContextIO (insertProject project7) appContext
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2), (c3, s3)) <- connectTestWebsocketUsers appContext project10.uuid
      ((c4, s4), (c5, s5), (c6, s6)) <- connectTestWebsocketUsers appContext project7.uuid
      -- WHEN:
      write_DeleteCommentThread c1 dtche_rQ1_t1'
      -- THEN:
      read_DeleteCommentThread c1 dte_rQ1_t1'
      read_DeleteCommentThread c2 dte_rQ1_t1'
      read_DeleteCommentThread c3 dte_rQ1_t1'
      nothingWasReceived c4
      nothingWasReceived c5
      nothingWasReceived c6
      -- AND: Close sockets
      closeSockets [s1, s2, s3, s4, s5, s6]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_DeleteCommentThread connection replyDto = do
  let reqDto = SetContent_ClientProjectMessageDTO replyDto
  sendMessage connection reqDto

read_DeleteCommentThread connection expReplyDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerProjectMessageDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerProjectMessageDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
