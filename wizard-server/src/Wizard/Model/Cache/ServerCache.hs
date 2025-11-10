module Wizard.Model.Cache.ServerCache where

import qualified Data.Cache as C

import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord
import WizardLib.Public.Model.User.UserToken

data ServerCache = ServerCache
  { knowledgeModelEditorWebsocket :: C.Cache Int WebsocketRecord
  , questionnaireWebsocket :: C.Cache Int WebsocketRecord
  , user :: C.Cache Int User
  , userToken :: C.Cache Int UserToken
  }
