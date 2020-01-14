module Wizard.Database.DAO.Metric.MetricDAO where

import Data.Bson

import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Helper (createHeeHelper)
import Wizard.Database.BSON.KnowledgeModel.KnowledgeModel ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext

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
