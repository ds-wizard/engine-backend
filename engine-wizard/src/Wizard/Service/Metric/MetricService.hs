module Wizard.Service.Metric.MetricService where

import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Metric.MetricMapper

getMetrics :: AppContextM [MetricDTO]
getMetrics = do
  metrics <- findMetrics
  return . fmap toMetricDTO $ metrics
