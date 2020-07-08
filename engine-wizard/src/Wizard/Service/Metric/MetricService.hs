module Wizard.Service.Metric.MetricService where

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Database.DAO.Metric.MetricDAO
import Wizard.Model.Context.AppContext

getMetrics :: AppContextM [Metric]
getMetrics = findMetrics
