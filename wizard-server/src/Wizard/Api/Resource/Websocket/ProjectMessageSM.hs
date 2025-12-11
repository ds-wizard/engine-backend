module Wizard.Api.Resource.Websocket.ProjectMessageSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Detail.ProjectDetailWsSM ()
import Wizard.Api.Resource.Project.Event.ProjectEventChangeSM ()
import Wizard.Api.Resource.Project.Event.ProjectEventSM ()
import Wizard.Api.Resource.Project.File.ProjectFileSimpleSM ()
import Wizard.Api.Resource.Project.ProjectReplySM ()
import Wizard.Api.Resource.User.OnlineUserInfoSM ()
import Wizard.Api.Resource.Websocket.ProjectMessageDTO
import Wizard.Api.Resource.Websocket.ProjectMessageJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectMessages

instance ToSchema ClientProjectMessageDTO where
  declareNamedSchema = toSwagger ensureOnlineUserAction

instance ToSchema ServerProjectMessageDTO where
  declareNamedSchema = toSwagger setUserListAction
