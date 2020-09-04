module Wizard.Model.Cache.ServerCache where

import qualified Data.Cache as C

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Report.Report
import Wizard.Model.Websocket.WebsocketRecord

data ServerCache =
  ServerCache
    { _serverCacheKnowledgeModel :: C.Cache Int KnowledgeModel
    , _serverCacheQuestionnaireReportIndications :: C.Cache Int [Indication]
    , _serverCacheQuestionnaireWebsocket :: C.Cache Int WebsocketRecord
    }
