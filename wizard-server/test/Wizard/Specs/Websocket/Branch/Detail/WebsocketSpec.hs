module Wizard.Specs.Websocket.Branch.Detail.WebsocketSpec where

import Test.Hspec hiding (shouldBe)

import Wizard.Model.Context.AppContext

import Wizard.Specs.Websocket.Branch.Detail.GeneralSpec
import Wizard.Specs.Websocket.Branch.Detail.SetEventSpec
import Wizard.Specs.Websocket.Branch.Detail.SetRepliesSpec

branchesWebsocketAPI :: AppContext -> SpecWith ()
branchesWebsocketAPI appContext =
  describe "WS /wizard-api/branches/{bUuid}/websocket" $ do
    generalSpec appContext
    setEventSpec appContext
    setRepliesSpec appContext
