module Wizard.Database.DAO.Metric.MetricDAO where

import GHC.Int

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.KnowledgeModel ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "metric"

findMetrics :: AppContextM [Metric]
findMetrics = createFindEntitiesFn entityName

insertMetric :: Metric -> AppContextM Int64
insertMetric = createInsertFn entityName

deleteMetrics :: AppContextM Int64
deleteMetrics = createDeleteEntitiesFn entityName

deleteMetricByUuid :: String -> AppContextM Int64
deleteMetricByUuid = createDeleteEntityByFn entityName "uuid"
