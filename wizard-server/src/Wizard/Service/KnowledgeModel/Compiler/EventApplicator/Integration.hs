module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Integration where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Question
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddIntegrationEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {integrationUuids = km.integrationUuids ++ [event.entityUuid]}
      addEntity = putInIntegrationsM event.entityUuid (createEntity event content)

instance ApplyEvent EditIntegrationEvent where
  apply event content = updateIntegrationVariables' content . applyEditEvent getIntegrationsM setIntegrationsM event content
    where
      updateIntegrationVariables' :: EditIntegrationEvent -> Either AppError KnowledgeModel -> Either AppError KnowledgeModel
      updateIntegrationVariables' _ (Left error) = Left error
      updateIntegrationVariables' content (Right km) = Right . setQuestionsM km $ M.map (updateIntegrationVariables event content) km.entities.questions

instance ApplyEvent DeleteIntegrationEvent where
  apply event content = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {integrationUuids = L.delete event.entityUuid km.integrationUuids}
      deleteEntity km = setIntegrationsM km $ M.delete event.entityUuid (getIntegrationsM km)
      deleteEntityChildrenReference km =
        setQuestionsM km $ M.map (deleteIntegrationReference event) km.entities.questions
