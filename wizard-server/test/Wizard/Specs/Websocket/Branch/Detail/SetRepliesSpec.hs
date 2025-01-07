module Wizard.Specs.Websocket.Branch.Detail.SetRepliesSpec where

import Data.Aeson
import Network.WebSockets
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Api.Resource.Websocket.BranchActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Database.Migration.Development.Branch.Data.BranchEvents
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchList

import Wizard.Specs.Websocket.Branch.Detail.Common
import Wizard.Specs.Websocket.Common

setRepliesSpec appContext = describe "setReplies" $ test200 appContext

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
      ((c1, s1), (c2, s2)) <- connectTestWebsocketUsers appContext branch.uuid
      ((c3, s3), (c4, s4)) <- connectTestWebsocketUsers appContext leidenBranch.uuid
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
  let reqDto = SetReplies_ClientBranchActionDTO replyDto
  sendMessage connection reqDto

read_SetReplies connection expRepliesDto = do
  resDto <- receiveData connection
  let eResult = eitherDecode resDto :: Either String (Success_ServerActionDTO ServerBranchActionDTO)
  let (Right (Success_ServerActionDTO (SetReplies_ServerBranchActionDTO replyDto))) = eResult
  expRepliesDto `shouldBe` replyDto
