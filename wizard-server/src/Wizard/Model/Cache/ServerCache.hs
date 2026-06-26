module Wizard.Model.Cache.ServerCache where

import qualified Data.Cache as C

import Wizard.Model.Websocket.WebsocketRecord
import WizardLib.Public.Model.User.UserToken

data ServerCache = ServerCache
  { knowledgeModelEditorWebsocket :: C.Cache Int WebsocketRecord
  , projectWebsocket :: C.Cache Int WebsocketRecord
  , userToken :: C.Cache Int UserToken
  }
