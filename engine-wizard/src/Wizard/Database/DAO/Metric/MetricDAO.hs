module Wizard.Database.DAO.Metric.MetricDAO where

import Data.Bson

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Database.BSON.KnowledgeModel.KnowledgeModel ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext

entityName = "metric"

collection = "metrics"

findMetrics :: AppContextM [Metric]
findMetrics = createFindEntitiesFn collection

insertMetric :: Metric -> AppContextM Value
insertMetric = createInsertFn collection

deleteMetrics :: AppContextM ()
deleteMetrics = createDeleteEntitiesFn collection

deleteMetricByUuid :: String -> AppContextM ()
deleteMetricByUuid = createDeleteEntityByFn collection "uuid"
