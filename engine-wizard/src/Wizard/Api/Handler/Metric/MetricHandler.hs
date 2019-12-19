module Wizard.Api.Handler.Metric.MetricHandler where

import Web.Scotty.Trans (json)

import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Wizard.Service.Metric.MetricService

getMetricsA :: Endpoint
getMetricsA =
  getAuthServiceExecutor $ \runInAuthService -> do
    eitherDto <- runInAuthService getMetrics
    case eitherDto of
      Right dto -> json dto
      Left error -> sendError error
