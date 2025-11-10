module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Metric where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddMetricEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {metricUuids = km.metricUuids ++ [event.entityUuid]}
      addEntity = putInMetricsM event.entityUuid (createEntity event content)

instance ApplyEvent EditMetricEvent where
  apply = applyEditEvent getMetricsM setMetricsM

instance ApplyEvent DeleteMetricEvent where
  apply event content = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {metricUuids = L.delete event.entityUuid km.metricUuids}
      deleteEntity km = deleteMetric km event.entityUuid
      deleteEntityChildrenReference km =
        setAnswersM km $ M.map (deleteMetricReference event) km.entities.answers
