module Wizard.Specs.Websocket.Questionnaire.Detail.WebsocketSpec where

import Test.Hspec hiding (shouldBe)

import Wizard.Model.Context.AppContext

import Wizard.Specs.Websocket.Questionnaire.Detail.AddCommentSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.ClearReplySpec
import Wizard.Specs.Websocket.Questionnaire.Detail.DeleteCommentSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.DeleteCommentThreadSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.EditCommentSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.GeneralSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.ReopenCommentThreadSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.ResolveCommentThreadSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.SetLabelsSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.SetPhaseSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.SetQuestionnaireSpec
import Wizard.Specs.Websocket.Questionnaire.Detail.SetReplySpec

questionnaireWebsocketAPI :: AppContext -> SpecWith ()
questionnaireWebsocketAPI appContext =
  describe "WS /wizard-api/questionnaires/{qtnUuid}/websocket" $ do
    generalSpec appContext
    setReplySpec appContext
    clearReplySpec appContext
    setPhaseSpec appContext
    setLabelsSpec appContext
    resolveCommentThreadSpec appContext
    reopenCommentThreadSpec appContext
    deleteCommentThreadSpec appContext
    addCommentSpec appContext
    editCommentSpec appContext
    deleteCommentSpec appContext
    setQuestionnaireSpec appContext
