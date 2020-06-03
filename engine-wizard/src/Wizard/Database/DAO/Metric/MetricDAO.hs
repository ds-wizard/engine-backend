module Wizard.Database.DAO.Metric.MetricDAO where

import Data.Bson

import Shared.Database.BSON.KnowledgeModel.KnowledgeModel ()
import Shared.Database.DAO.Common
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextLenses ()

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
