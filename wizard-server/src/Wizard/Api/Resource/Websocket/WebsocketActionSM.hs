module Wizard.Api.Resource.Websocket.WebsocketActionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Websocket.ProjectMessageSM ()
import Wizard.Api.Resource.Websocket.WebsocketActionDTO
import Wizard.Api.Resource.Websocket.WebsocketActionJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectMessages

instance ToSchema resDto => ToSchema (Success_ServerActionDTO resDto) where
  declareNamedSchema = toSwagger (Success_ServerActionDTO ensureOnlineUserAction)
