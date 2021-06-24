module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Metric where

import Control.Lens
import qualified Data.Map as M
import Prelude hiding (lookup)

import LensesConfig
import Shared.Model.Event.EventLenses
import Shared.Model.Event.Metric.MetricEvent
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()
import Wizard.Util.Lens

instance ApplyEvent AddMetricEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km & ap metricUuids .~ (event ^. entityUuid')
      addEntity km = km & metricsM . at (event ^. entityUuid') ?~ createEntity event

instance ApplyEvent EditMetricEvent where
  apply = applyEditEvent (entities . metrics) "Metric"

instance ApplyEvent DeleteMetricEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km & del metricUuids .~ (event ^. entityUuid')
      deleteEntity km = deleteMetric km (event ^. entityUuid')
      deleteEntityChildrenReference km =
        km & entities . answers .~ M.map (deleteMetricReference event) (km ^. entities . answers)
