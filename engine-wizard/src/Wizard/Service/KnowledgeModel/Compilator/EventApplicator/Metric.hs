module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Metric where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.Event.EventLenses
import Shared.Model.Event.Metric.MetricEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
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

instance ApplyEvent AddMetricEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {metricUuids = km.metricUuids ++ [getEntityUuid event]}
      addEntity = putInMetricsM (getEntityUuid event) (createEntity event)

instance ApplyEvent EditMetricEvent where
  apply = applyEditEvent getMetricsM setMetricsM

instance ApplyEvent DeleteMetricEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {metricUuids = L.delete (getEntityUuid event) km.metricUuids}
      deleteEntity km = deleteMetric km (getEntityUuid event)
      deleteEntityChildrenReference km =
        setAnswersM km $ M.map (deleteMetricReference event) km.entities.answers
