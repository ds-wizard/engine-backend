module Wizard.Model.Cache.ServerCache where

import qualified Data.Cache as C

import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.Public.Model.User.UserToken

data ServerCache = ServerCache
  { branchWebsocket :: C.Cache Int WebsocketRecord
  , knowledgeModel :: C.Cache Int KnowledgeModel
  , questionnaireWebsocket :: C.Cache Int WebsocketRecord
  , user :: C.Cache Int User
  , userToken :: C.Cache Int UserToken
  }
