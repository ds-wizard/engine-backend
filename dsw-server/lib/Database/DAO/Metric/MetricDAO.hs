module Database.DAO.Metric.MetricDAO where

import Data.Bson

import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Model.KnowledgeModel.KnowledgeModel
import Util.Helper (createHeeHelper)

entityName = "metric"

collection = "metrics"

findMetrics :: AppContextM (Either AppError [Metric])
findMetrics = createFindEntitiesFn collection

insertMetric :: Metric -> AppContextM Value
insertMetric = createInsertFn collection

deleteMetrics :: AppContextM ()
deleteMetrics = createDeleteEntitiesFn collection

deleteMetricByUuid :: String -> AppContextM ()
deleteMetricByUuid = createDeleteEntityByFn collection "uuid"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindMetrics callback = createHeeHelper findMetrics callback
