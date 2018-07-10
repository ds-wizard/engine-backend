module Service.Metric.MetricService where

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Common.Error
import Database.DAO.Metric.MetricDAO
import Model.Context.AppContext
import Service.Metric.MetricMapper

getMetrics :: AppContextM (Either AppError [MetricDTO])
getMetrics = heFindMetrics $ \metrics -> return . Right $ toDTO <$> metrics

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetMetrics callback = do
  eitherMetrics <- getMetrics
  case eitherMetrics of
    Right metrics -> callback metrics
    Left error -> return . Left $ error
