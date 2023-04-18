module Wizard.Model.Cache.ServerCache where

import qualified Data.Cache as C

import Wizard.Model.User.User
import Wizard.Model.User.UserToken
import Wizard.Model.Websocket.WebsocketRecord

data ServerCache = ServerCache
  { branchWebsocket :: C.Cache Int WebsocketRecord
  , questionnaireWebsocket :: C.Cache Int WebsocketRecord
  , user :: C.Cache Int User
  , userToken :: C.Cache Int UserToken
  }
