module Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.WebsocketSpec where

import Test.Hspec hiding (shouldBe)

import Wizard.Model.Context.AppContext

import Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.GeneralSpec
import Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.SetEventSpec
import Wizard.Specs.Websocket.KnowledgeModelEditor.Detail.SetRepliesSpec

knowledgeModelEditorWebsocketAPI :: AppContext -> SpecWith ()
knowledgeModelEditorWebsocketAPI appContext =
  describe "WS /wizard-api/knowledge-model-editors/{uuid}/websocket" $ do
    generalSpec appContext
    setEventSpec appContext
    setRepliesSpec appContext
