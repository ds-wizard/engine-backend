module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Model.Error.Error
import Shared.Model.Event.EventLenses
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Choice ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Question ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Tag ()

_NODE_NOT_FOUND nodeType node =
  "Node (type: " ++ nodeType ++ ", uuid: " ++ U.toString (getEntityUuid node) ++ ") was not found"

class ApplyEvent a where
  apply :: a -> KnowledgeModel -> Either AppError KnowledgeModel

applyCreateEventWithParent getEntityCol setEntityCol getParentCol setParentCol getParentUuidCol setParentUuidCol event km =
  case M.lookup (getParentUuid event) (getParentCol km) of
    Nothing -> Right km
    Just parentEntity -> Right . addEntityReference parentEntity . addEntity $ km
  where
    addEntityReference ch km = setParentCol km $ M.insert (getUuid ch) (setParentUuidCol ch (getParentUuidCol ch ++ [getEntityUuid event])) (getParentCol km)
    addEntity km = setEntityCol km $ M.insert (getEntityUuid event) (createEntity event) (getEntityCol km)

applyEditEvent getEntityCol setEntityCol event km =
  case M.lookup (getEntityUuid event) (getEntityCol km) of
    Nothing -> Right km
    Just entity -> Right . updateEntity km $ entity
  where
    updateEntity km entity =
      setEntityCol km $ M.insert (getEntityUuid event) (editEntity event entity) (getEntityCol km)

-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
deleteEntityReferenceFromParentNode event getParentCollectionInEntities setParentCollectionInEntities getParentCollectionOfChildUuids setParentCollectionOfChildUuids km =
  case M.lookup (getParentUuid event) (getParentCollectionInEntities km) of
    Nothing -> Right km
    Just parentNode -> Right . removeEntityReference $ parentNode
  where
    removeEntityReference parentNode =
      setParentCollectionInEntities km $ M.insert (getUuid parentNode) (setParentCollectionOfChildUuids parentNode $ L.delete (getEntityUuid event) (getParentCollectionOfChildUuids parentNode)) (getParentCollectionInEntities km)
