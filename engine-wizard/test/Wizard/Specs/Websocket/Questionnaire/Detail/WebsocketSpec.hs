module Wizard.Specs.Websocket.Questionnaire.Detail.WebsocketSpec where

import Test.Hspec hiding (shouldBe)

import Wizard.Model.Context.AppContext

import Wizard.Specs.Websocket.Questionnaire.Detail.ClearReplySpec
import Wizard.Specs.Websocket.Questionnaire.Detail.GeneralSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.SetLabelsSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.SetPhaseSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.SetReplySpec

questionnaireWebsocketAPI :: AppContext -> SpecWith ()
questionnaireWebsocketAPI appContext =
  describe "WS /questionnaires/{qtnUuid}/websocket" $ do
    generalSpec appContext
    setReplySpec appContext
    clearReplySpec appContext
    setPhaseSpec appContext
    setLabelsSpec appContext
