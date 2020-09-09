module Wizard.Api.Handler.Metric.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Metric.MetricService

type List_GET
   = "metrics"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [Metric])

list_GET :: BaseContextM (Headers '[ Header "x-trace-uuid" String] [Metric])
list_GET = runInUnauthService $ addTraceUuidHeader =<< getMetrics
