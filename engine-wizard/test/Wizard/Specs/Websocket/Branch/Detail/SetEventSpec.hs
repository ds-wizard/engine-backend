module Wizard.Specs.Websocket.Branch.Detail.SetEventSpec where

import Control.Lens ((^.))
import Data.Aeson
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Wizard.Api.Resource.Websocket.BranchActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.Migration.Development.Branch.Data.BranchEvents
import Wizard.Database.Migration.Development.Branch.Data.Branches

import Wizard.Specs.Websocket.Branch.Detail.Common
import Wizard.Specs.Websocket.Common

setEventSpec appContext = describe "setEvent" $ test200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test200 appContext =
  it "WS 200 OK" $
    -- GIVEN: Prepare database
   do
    let branch = amsterdamBranch
    let branchData = amsterdamBranchData
    insertBranchAndUsers appContext branch branchData
    -- AND: Connect to websocket
    ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers appContext (branch ^. uuid)
    ((c3, s3), (c4, s4)) <- connectTestWebsocketUsers appContext (leidenBranch ^. uuid)
    -- WHEN:
    write_SetReply c1 branchEvent1'
    -- THEN:
    read_SetReply c1 branchEvent1'
    read_SetReply c2 branchEvent1'
    nothingWasReceived c3
    nothingWasReceived c4
    -- AND: Close sockets
    closeSockets [s1, s2]
    closeSockets [s1, s2, s3, s4]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
write_SetReply connection replyDto = do
  let reqDto = SetContent_ClientBranchActionDTO replyDto
  sendMessage connection reqDto

read_SetReply connection expReplyDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerBranchActionDTO)
  let (Right (Success_ServerActionDTO (SetContent_ServerBranchActionDTO replyDto))) = eResult
  expReplyDto `shouldBe` replyDto
