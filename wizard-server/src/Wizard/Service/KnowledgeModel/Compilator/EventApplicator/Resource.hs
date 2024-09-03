module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.Resource where

import qualified Data.List as L
import qualified Data.UUID as U
import Prelude hiding (lookup)

import Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Resource ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEvent
import WizardLib.KnowledgeModel.Model.Event.Resource.ResourceEventLenses ()
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessors

instance ApplyEvent AddResourceCollectionEvent where
  apply event = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {resourceCollectionUuids = km.resourceCollectionUuids ++ [getEntityUuid event]}
      addEntity = putInResourceCollectionsM (getEntityUuid event) (createEntity event)

instance ApplyEvent EditResourceCollectionEvent where
  apply = applyEditEvent getResourceCollectionsM setResourceCollectionsM

instance ApplyEvent DeleteResourceCollectionEvent where
  apply event = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {resourceCollectionUuids = L.delete (getEntityUuid event) km.resourceCollectionUuids}
      deleteEntity km = deleteResourceCollection km (getEntityUuid event)
      deleteEntityChildrenReference km =
        let resourcePageUuidsToBeDeleted = getResourcePagesUuidsForResourceCollectionUuid km event.entityUuid
         in setResourcePagesL km $ filter (\rp -> rp.uuid `notElem` resourcePageUuidsToBeDeleted) (getResourcePagesL km)

-- --------------------------------------------
instance ApplyEvent AddResourcePageEvent where
  apply = applyCreateEventWithParent getResourcePagesM setResourcePagesM getResourceCollectionsM setResourceCollectionsM (.resourcePageUuids) setPageUuids

instance ApplyEvent EditResourcePageEvent where
  apply = applyEditEvent getResourcePagesM setResourcePagesM

instance ApplyEvent DeleteResourcePageEvent where
  apply event km =
    deleteEntityReferenceFromParentNode event getResourceCollectionsM setResourceCollectionsM (.resourcePageUuids) setPageUuids $ deleteResourcePage km (getEntityUuid event)

setPageUuids :: ResourceCollection -> [U.UUID] -> ResourceCollection
setPageUuids entity uuids = entity {resourcePageUuids = uuids}
