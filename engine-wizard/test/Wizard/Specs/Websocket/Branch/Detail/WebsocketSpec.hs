module Wizard.Specs.Websocket.Branch.Detail.WebsocketSpec where

import Test.Hspec hiding (shouldBe)

import Wizard.Model.Context.AppContext

import Wizard.Specs.Websocket.Branch.Detail.GeneralSpec
import Wizard.Specs.Websocket.Branch.Detail.SetEventSpec

branchesWebsocketAPI :: AppContext -> SpecWith ()
branchesWebsocketAPI appContext =
  describe "WS /branches/{bUuid}/websocket" $ do
    generalSpec appContext
    setEventSpec appContext
