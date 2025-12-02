module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.Resource where

import qualified Data.List as L
import qualified Data.UUID as U
import Prelude hiding (lookup)

import Shared.KnowledgeModel.Model.Common.Lens
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Resource.ResourceEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessors
import Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Delete
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Resource ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

instance ApplyEvent AddResourceCollectionEvent where
  apply event content = Right . addEntity . addEntityReference
    where
      addEntityReference km = km {resourceCollectionUuids = km.resourceCollectionUuids ++ [event.entityUuid]}
      addEntity = putInResourceCollectionsM event.entityUuid (createEntity event content)

instance ApplyEvent EditResourceCollectionEvent where
  apply = applyEditEvent getResourceCollectionsM setResourceCollectionsM

instance ApplyEvent DeleteResourceCollectionEvent where
  apply event content = Right . deleteEntity . deleteEntityReference . deleteEntityChildrenReference
    where
      deleteEntityReference km = km {resourceCollectionUuids = L.delete event.entityUuid km.resourceCollectionUuids}
      deleteEntity km = deleteResourceCollection km event.entityUuid
      deleteEntityChildrenReference km =
        let resourcePageUuidsToBeDeleted = getResourcePagesUuidsForResourceCollectionUuid km event.entityUuid
         in setResourcePagesL km $ filter (\rp -> rp.uuid `notElem` resourcePageUuidsToBeDeleted) (getResourcePagesL km)

-- --------------------------------------------
instance ApplyEvent AddResourcePageEvent where
  apply = applyCreateEventWithParent getResourcePagesM setResourcePagesM getResourceCollectionsM setResourceCollectionsM (.resourcePageUuids) setPageUuids

instance ApplyEvent EditResourcePageEvent where
  apply = applyEditEvent getResourcePagesM setResourcePagesM

instance ApplyEvent DeleteResourcePageEvent where
  apply event content km =
    deleteEntityReferenceFromParentNode event getResourceCollectionsM setResourceCollectionsM (.resourcePageUuids) setPageUuids $ deleteResourcePage km event.entityUuid

setPageUuids :: ResourceCollection -> [U.UUID] -> ResourceCollection
setPageUuids entity uuids = entity {resourcePageUuids = uuids}
