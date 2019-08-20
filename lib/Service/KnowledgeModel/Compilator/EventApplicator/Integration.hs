module Service.KnowledgeModel.Compilator.EventApplicator.Integration where

import Control.Lens
import qualified Data.Map as M
import Prelude hiding (lookup)

import LensesConfig
import Model.Error.Error
import Model.Event.EventAccessors
import Model.Event.Integration.IntegrationEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Service.KnowledgeModel.Compilator.Modifier.Modifier
import Service.KnowledgeModel.Compilator.Modifier.Question
import Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Service.KnowledgeModel.Compilator.Modifier.Tag ()
import Util.Lens

instance ApplyEvent AddIntegrationEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km & (ap integrationUuids) .~ (getEventNodeUuid event)
      addEntity km = km & (integrationsM . at (getEventNodeUuid event)) ?~ (createEntity event)

instance ApplyEvent EditIntegrationEvent where
  apply event = updateIntegrationProps' event . applyEditEvent (entities . integrations) "Integration" event
    where
      updateIntegrationProps' ::
           EditIntegrationEvent -> Either AppError KnowledgeModel -> Either AppError KnowledgeModel
      updateIntegrationProps' _ (Left error) = Left error
      updateIntegrationProps' event (Right km) =
        Right $ km & (entities . questions) .~ (M.map (updateIntegrationProps event) (km ^. entities . questions))

instance ApplyEvent DeleteIntegrationEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km & del integrationUuids .~ (getEventNodeUuid event)
      deleteEntity km = km & integrationsM .~ (M.delete (getEventNodeUuid event) (km ^. integrationsM))
      deleteEntityChildrenReference km =
        km & (entities . questions) .~ (M.map (deleteIntegrationReference event) (km ^. entities . questions))
