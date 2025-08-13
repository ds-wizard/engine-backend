module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Integration where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Common.Model.Error.Error
import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance ApplyEvent AddIntegrationEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {integrationUuids = km.integrationUuids ++ [getEntityUuid event]}
      addEntity = putInIntegrationsM (getEntityUuid event) (createEntity event)

instance ApplyEvent EditIntegrationEvent where
  apply event = updateIntegrationVariables' event . applyEditEvent getIntegrationsM setIntegrationsM event
    where
      updateIntegrationVariables' :: EditIntegrationEvent -> Either AppError KnowledgeModel -> Either AppError KnowledgeModel
      updateIntegrationVariables' _ (Left error) = Left error
      updateIntegrationVariables' event (Right km) = Right . setQuestionsM km $ M.map (updateIntegrationVariables event) km.entities.questions

instance ApplyEvent DeleteIntegrationEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {integrationUuids = L.delete (getEntityUuid event) km.integrationUuids}
      deleteEntity km = setIntegrationsM km $ M.delete (getEntityUuid event) (getIntegrationsM km)
      deleteEntityChildrenReference km =
        setQuestionsM km $ M.map (deleteIntegrationReference event) km.entities.questions
