module Wizard.Specs.Websocket.Project.Detail.WebsocketSpec where

import Test.Hspec hiding (shouldBe)

import Wizard.Model.Context.AppContext

import Wizard.Specs.Websocket.Project.Detail.AddCommentSpec
import Wizard.Specs.Websocket.Project.Detail.AssignCommentThreadSpec
import Wizard.Specs.Websocket.Project.Detail.ClearReplySpec
import Wizard.Specs.Websocket.Project.Detail.DeleteCommentSpec
import Wizard.Specs.Websocket.Project.Detail.DeleteCommentThreadSpec
import Wizard.Specs.Websocket.Project.Detail.EditCommentSpec
import Wizard.Specs.Websocket.Project.Detail.GeneralSpec
import Wizard.Specs.Websocket.Project.Detail.ReopenCommentThreadSpec
import Wizard.Specs.Websocket.Project.Detail.ResolveCommentThreadSpec
import Wizard.Specs.Websocket.Project.Detail.SetLabelsSpec
import Wizard.Specs.Websocket.Project.Detail.SetPhaseSpec
import Wizard.Specs.Websocket.Project.Detail.SetProjectSpec
import Wizard.Specs.Websocket.Project.Detail.SetReplySpec

projectWebsocketAPI :: AppContext -> SpecWith ()
projectWebsocketAPI appContext =
  describe "WS /wizard-api/projects/{projectUuid}/websocket" $ do
    generalSpec appContext
    setReplySpec appContext
    clearReplySpec appContext
    setPhaseSpec appContext
    setLabelsSpec appContext
    resolveCommentThreadSpec appContext
    reopenCommentThreadSpec appContext
    assignCommentThreadSpec appContext
    deleteCommentThreadSpec appContext
    addCommentSpec appContext
    editCommentSpec appContext
    deleteCommentSpec appContext
    setProjectSpec appContext
