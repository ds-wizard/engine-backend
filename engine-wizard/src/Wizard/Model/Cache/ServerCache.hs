module Wizard.Model.Cache.ServerCache where

import qualified Data.Cache as C
import qualified Data.Map.Strict as M

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Report.Report
import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord

data ServerCache =
  ServerCache
    { _serverCacheKnowledgeModel :: C.Cache Int (M.Map Int KnowledgeModel)
    , _serverCacheQuestionnaireContent :: C.Cache Int QuestionnaireContent
    , _serverCacheQuestionnaireReportIndications :: C.Cache Int [Indication]
    , _serverCacheQuestionnaireWebsocket :: C.Cache Int WebsocketRecord
    , _serverCacheUser :: C.Cache Int User
    , _serverCachePackage :: C.Cache Int Package
    }
