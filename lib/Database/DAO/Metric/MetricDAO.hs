module Database.DAO.Metric.MetricDAO where

import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, deleteOne, find, findOne, insert, rest, select)

import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Database.DAO.Common
import Model.Context.AppContext
import Model.Error.Error
import Model.KnowledgeModel.KnowledgeModel

metricCollection = "metrics"

findMetrics :: AppContextM (Either AppError [Metric])
findMetrics = do
  let action = rest =<< find (select [] metricCollection)
  metricsS <- runDB action
  return . deserializeEntities $ metricsS

findMetricByShortUuid :: String -> AppContextM (Either AppError Metric)
findMetricByShortUuid uuid = do
  let action = findOne $ select ["uuid" =: uuid] metricCollection
  maybeMetricS <- runDB action
  return . deserializeMaybeEntity $ maybeMetricS

insertMetric :: Metric -> AppContextM Value
insertMetric metric = do
  let action = insert metricCollection (toBSON metric)
  runDB action

deleteMetrics :: AppContextM ()
deleteMetrics = do
  let action = delete $ select [] metricCollection
  runDB action

deleteMetricByUuid :: String -> AppContextM ()
deleteMetricByUuid uuid = do
  let action = deleteOne $ select ["uuid" =: uuid] metricCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindMetrics callback = do
  eitherMetrics <- findMetrics
  case eitherMetrics of
    Right metrics -> callback metrics
    Left error -> return . Left $ error
