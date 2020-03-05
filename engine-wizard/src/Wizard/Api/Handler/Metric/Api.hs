module Wizard.Api.Handler.Metric.Api where

import Servant

import Wizard.Api.Handler.Metric.List_GET
import Wizard.Model.Context.BaseContext

type MetricAPI = List_GET

metricApi :: Proxy MetricAPI
metricApi = Proxy

metricServer :: ServerT MetricAPI BaseContextM
metricServer = list_GET
