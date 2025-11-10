module Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.SetEventSpec where

import Data.Aeson
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Api.Resource.Websocket.KnowledgeModelEditorActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList

import Wizard.Specs.Websocket.Common
import Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.Common

setEventSpec appContext = describe "setEvent" $ test200 appContext

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
      write_SetReply c1 knowledgeModelEditorWebsocketEvent1'
      -- THEN:
      read_SetReply c1 knowledgeModelEditorWebsocketEvent1'
      read_SetReply c2 knowledgeModelEditorWebsocketEvent1'
      nothingWasReceived c3
      nothingWasReceived c4
      -- AND: Close sockets
      closeSockets [s1, s2]
      closeSockets [s1, s2, s3, s4]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_SetReply connection replyDto = do
  let reqDto = SetContent_ClientKnowledgeModelEditorActionDTO replyDto
  sendMessage connection reqDto

read_SetReply connection expReplyDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerKnowledgeModelEditorActionDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerKnowledgeModelEditorActionDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
