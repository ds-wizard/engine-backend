module Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.SetRepliesSpec where

import Data.Aeson
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList

import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.Common

setRepliesSpec appContext = describe "setReplies" $ test200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 appContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
    do
      let editor = amsterdamKnowledgeModelEditor
      insertKnowledgeModelEditorAndUsers appContext editor
      -- AND: Connect to websocket
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers appContext editor.uuid
      ((c3, s3), (c4, s4)) <- connectTestWebsocketUsers appContext leidenKnowledgeModelEditor.uuid
      -- WHEN:
      write_SetReplies c1 setRepliesDTO
      -- THEN:
      read_SetReplies c1 setRepliesDTO
      read_SetReplies c2 setRepliesDTO
      nothingWasReceived c3
      nothingWasReceived c4
      -- AND: Close sockets
      closeSockets [s1, s2]
      closeSockets [s1, s2, s3, s4]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_SetReplies connection replyDto = do
  let reqDto = SetReplies_ClientKnowledgeModelEditorMessageDTO replyDto
  sendMessage connection reqDto

read_SetReplies connection expRepliesDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerKnowledgeModelEditorMessageDTO)
  let (Right (Success_ServerActionDTO (SetReplies_ServerKnowledgeModelEditorMessageDTO replyDto))) = eResult
  expRepliesDto `shouldBe` replyDto
