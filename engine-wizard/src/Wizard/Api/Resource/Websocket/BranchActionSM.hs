module Wizard.Api.Resource.Websocket.BranchActionSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Branch.Event.BranchEventSM ()
import Wizard.Api.Resource.User.OnlineUserInfoSM ()
import Wizard.Api.Resource.Websocket.BranchActionDTO
import Wizard.Api.Resource.Websocket.BranchActionJM ()
import Wizard.Database.Migration.Development.Branch.Data.BranchActions

instance ToSchema ClientBranchActionDTO where
  declareNamedSchema = simpleToSchema ensureOnlineUserAction

instance ToSchema ServerBranchActionDTO where
  declareNamedSchema = simpleToSchema setUserListAction
