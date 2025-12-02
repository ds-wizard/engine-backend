module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Choice ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Question ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()

_NODE_NOT_FOUND nodeType node =
  "Node (type: " ++ nodeType ++ ", uuid: " ++ U.toString node.entityUuid ++ ") was not found"

class ApplyEvent a where
  apply :: KnowledgeModelEvent -> a -> KnowledgeModel -> Either AppError KnowledgeModel

applyCreateEventWithParent getEntityCol setEntityCol getParentCol setParentCol getParentUuidCol setParentUuidCol event content km =
  case M.lookup event.parentUuid (getParentCol km) of
    Nothing -> Right km
    Just parentEntity -> Right . addEntityReference parentEntity . addEntity $ km
  where
    addEntityReference entity km = setParentCol km $ M.insert (getUuid entity) (setParentUuidCol entity (getParentUuidCol entity ++ [event.entityUuid])) (getParentCol km)
    addEntity km = setEntityCol km $ M.insert event.entityUuid (createEntity event content) (getEntityCol km)

applyEditEvent getEntityCol setEntityCol event content km =
  case M.lookup event.entityUuid (getEntityCol km) of
    Nothing -> Right km
    Just entity -> Right . updateEntity km $ entity
  where
    updateEntity km entity =
      setEntityCol km $ M.insert event.entityUuid (editEntity event content entity) (getEntityCol km)

-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
deleteEntityReferenceFromParentNode event getParentCollectionInEntities setParentCollectionInEntities getParentCollectionOfChildUuids setParentCollectionOfChildUuids km =
  case M.lookup event.parentUuid (getParentCollectionInEntities km) of
    Nothing -> Right km
    Just parentNode -> Right . removeEntityReference $ parentNode
  where
    removeEntityReference parentNode =
      setParentCollectionInEntities km $ M.insert (getUuid parentNode) (setParentCollectionOfChildUuids parentNode $ L.delete event.entityUuid (getParentCollectionOfChildUuids parentNode)) (getParentCollectionInEntities km)
