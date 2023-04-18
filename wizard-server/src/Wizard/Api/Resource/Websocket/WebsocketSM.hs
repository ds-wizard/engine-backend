module Wizard.Api.Resource.Websocket.WebsocketSM where

import Servant.API.WebSocket
import Servant.Swagger

instance HasSwagger WebSocket where
  toSwagger _ = mempty

instance HasSwagger WebSocketPending where
  toSwagger _ = mempty
