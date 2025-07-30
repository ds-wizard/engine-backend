module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Metric where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

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
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Metric.MetricEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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
