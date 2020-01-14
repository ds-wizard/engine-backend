module Wizard.Service.Metric.MetricService where

import Shared.Model.Error.Error
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Metric.MetricMapper

getMetrics :: AppContextM (Either AppError [MetricDTO])
getMetrics = heFindMetrics $ \metrics -> return . Right $ toMetricDTO <$> metrics

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetMetrics callback = do
  eitherMetrics <- getMetrics
  case eitherMetrics of
    Right metrics -> callback metrics
    Left error -> return . Left $ error
