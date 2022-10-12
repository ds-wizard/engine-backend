module Wizard.Model.Cache.ServerCache where

import qualified Data.Cache as C

import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord

data ServerCache =
  ServerCache
    { _serverCacheBranchWebsocket :: C.Cache Int WebsocketRecord
    , _serverCacheQuestionnaireWebsocket :: C.Cache Int WebsocketRecord
    , _serverCacheUser :: C.Cache Int User
    }
