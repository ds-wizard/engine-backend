module Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator where

import Control.Lens
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.UUID as U

import Model.Error.Error
import Model.Event.EventAccessors
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelLenses
import Service.KnowledgeModel.Compilator.Modifier.Answer ()
import Service.KnowledgeModel.Compilator.Modifier.Chapter ()
import Service.KnowledgeModel.Compilator.Modifier.Expert ()
import Service.KnowledgeModel.Compilator.Modifier.Integration ()
import Service.KnowledgeModel.Compilator.Modifier.KnowledgeModel ()
import Service.KnowledgeModel.Compilator.Modifier.Modifier
import Service.KnowledgeModel.Compilator.Modifier.Question ()
import Service.KnowledgeModel.Compilator.Modifier.Reference ()
import Service.KnowledgeModel.Compilator.Modifier.Tag ()

_NODE_NOT_FOUND nodeType node =
  "Node (type: " ++ nodeType ++ ", uuid: " ++ (U.toString . getEventNodeUuid $ node) ++ ") was not found"

class ApplyEvent a where
  apply :: a -> KnowledgeModel -> Either AppError KnowledgeModel

applyCreateEvent ::
     (HasUuid' a, EventAccesors c, CreateEntity c a)
  => Lens' KnowledgeModel (M.Map U.UUID a)
  -> Lens' KnowledgeModel [U.UUID]
  -> String
  -> c
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
applyCreateEvent entityCol parentUuidCol entityName event km = Right . addEntityReference . addEntity $ km
  where
    addEntityReference km = km & parentUuidCol .~ ((km ^. parentUuidCol) ++ [getEventNodeUuid event])
    addEntity km = km & entityCol .~ (M.insert (getEventNodeUuid event) (createEntity event) (km ^. entityCol))

applyCreateEventWithParent ::
     (HasUuid' a, HasUuid' b, EventAccesors c, CreateEntity c a)
  => Lens' KnowledgeModel (M.Map U.UUID a)
  -> Lens' KnowledgeModel (M.Map U.UUID b)
  -> Lens' b [U.UUID]
  -> String
  -> String
  -> c
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
applyCreateEventWithParent entityCol parentCol parentUuidCol entityName parentName event km =
  case M.lookup (getEventParentUuid event) (km ^. parentCol) of
    Nothing -> Left . GeneralServerError . _NODE_NOT_FOUND parentName $ event
    Just parentEntity -> Right . addEntityReference parentEntity . addEntity $ km
  where
    addEntityReference ch km =
      km & parentCol .~
      (M.insert
         (ch ^. uuid')
         (ch & parentUuidCol .~ ((ch ^. parentUuidCol) ++ [getEventNodeUuid event]))
         (km ^. parentCol))
    addEntity km = km & entityCol .~ (M.insert (getEventNodeUuid event) (createEntity event) (km ^. entityCol))

applyEditEvent ::
     (HasUuid' a, EditEntity c a, EventAccesors c)
  => Lens' KnowledgeModel (M.Map U.UUID a)
  -> String
  -> c
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
applyEditEvent entityCol entityName event km =
  case M.lookup (getEventNodeUuid event) (km ^. entityCol) of
    Nothing -> Left . GeneralServerError . _NODE_NOT_FOUND entityName $ event
    Just entity -> Right . updateEntity km $ entity
  where
    updateEntity km entity =
      km & entityCol .~ (M.insert (getEventNodeUuid event) (editEntity event entity) (km ^. entityCol))

applyDeleteEvent ::
     (HasUuid' a, EventAccesors c)
  => Lens' KnowledgeModel (M.Map U.UUID a)
  -> Lens' KnowledgeModel [U.UUID]
  -> String
  -> c
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
applyDeleteEvent entityCol parentUuidCol entityName event km =
  case M.lookup (getEventNodeUuid event) (km ^. entityCol) of
    Nothing -> Left . GeneralServerError . _NODE_NOT_FOUND entityName $ event
    Just ch -> Right . removeEntityReference . removeEntity $ km
  where
    removeEntityReference km = km & parentUuidCol .~ (L.delete (getEventNodeUuid event) (km ^. parentUuidCol))
    removeEntity km = km & entityCol .~ (M.delete (getEventNodeUuid event) (km ^. entityCol))

-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
deleteEntityReferenceFromParentNode ::
     (HasUuid' parentEntity, EventAccesors event)
  => event
  -> Lens' KnowledgeModel (M.Map U.UUID parentEntity)
  -> Lens' parentEntity [U.UUID]
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
deleteEntityReferenceFromParentNode event parentCollectionInEntities parentCollectionOfChildUuids km =
  case M.lookup (getEventParentUuid event) (km ^. parentCollectionInEntities) of
    Nothing -> Right km
    Just parentNode -> Right . removeEntityReference $ parentNode
  where
    removeEntityReference parentNode =
      km & parentCollectionInEntities .~
      (M.insert
         (parentNode ^. uuid')
         (parentNode & parentCollectionOfChildUuids .~
          (L.delete (getEventNodeUuid event) (parentNode ^. parentCollectionOfChildUuids)))
         (km ^. parentCollectionInEntities))
