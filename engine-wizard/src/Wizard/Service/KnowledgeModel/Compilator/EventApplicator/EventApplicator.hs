module Wizard.Service.KnowledgeModel.Compilator.EventApplicator.EventApplicator where

import Control.Lens
import qualified Data.List as L
import qualified Data.Map as M
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
  "Node (type: " ++ nodeType ++ ", uuid: " ++ U.toString (node ^. entityUuid') ++ ") was not found"

class ApplyEvent a where
  apply :: a -> KnowledgeModel -> Either AppError KnowledgeModel

applyCreateEvent ::
     (HasUuid' a, HasParentUuid' c, HasEntityUuid' c, CreateEntity c a)
  => Lens' KnowledgeModel (M.Map U.UUID a)
  -> Lens' KnowledgeModel [U.UUID]
  -> String
  -> c
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
applyCreateEvent entityCol parentUuidCol entityName event = Right . addEntityReference . addEntity
  where
    addEntityReference km = km & parentUuidCol .~ ((km ^. parentUuidCol) ++ [event ^. entityUuid'])
    addEntity km = km & entityCol .~ M.insert (event ^. entityUuid') (createEntity event) (km ^. entityCol)

applyCreateEventWithParent ::
     (HasUuid' a, HasUuid' b, HasParentUuid' c, HasEntityUuid' c, CreateEntity c a)
  => Lens' KnowledgeModel (M.Map U.UUID a)
  -> Lens' KnowledgeModel (M.Map U.UUID b)
  -> Lens' b [U.UUID]
  -> String
  -> String
  -> c
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
applyCreateEventWithParent entityCol parentCol parentUuidCol entityName parentName event km =
  case M.lookup (event ^. parentUuid') (km ^. parentCol) of
    Nothing -> Right km
    Just parentEntity -> Right . addEntityReference parentEntity . addEntity $ km
  where
    addEntityReference ch km =
      km &
      parentCol .~
      M.insert (ch ^. uuid') (ch & parentUuidCol .~ ((ch ^. parentUuidCol) ++ [event ^. entityUuid'])) (km ^. parentCol)
    addEntity km = km & entityCol .~ M.insert (event ^. entityUuid') (createEntity event) (km ^. entityCol)

applyEditEvent ::
     (HasUuid' a, EditEntity c a, HasParentUuid' c, HasEntityUuid' c)
  => Lens' KnowledgeModel (M.Map U.UUID a)
  -> String
  -> c
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
applyEditEvent entityCol entityName event km =
  case M.lookup (event ^. entityUuid') (km ^. entityCol) of
    Nothing -> Right km
    Just entity -> Right . updateEntity km $ entity
  where
    updateEntity km entity =
      km & entityCol .~ M.insert (event ^. entityUuid') (editEntity event entity) (km ^. entityCol)

applyDeleteEvent ::
     (HasUuid' a, HasParentUuid' c, HasEntityUuid' c)
  => Lens' KnowledgeModel (M.Map U.UUID a)
  -> Lens' KnowledgeModel [U.UUID]
  -> String
  -> c
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
applyDeleteEvent entityCol parentUuidCol entityName event km =
  case M.lookup (event ^. entityUuid') (km ^. entityCol) of
    Nothing -> Right km
    Just ch -> Right . removeEntityReference . removeEntity $ km
  where
    removeEntityReference km = km & parentUuidCol .~ L.delete (event ^. entityUuid') (km ^. parentUuidCol)
    removeEntity km = km & entityCol .~ M.delete (event ^. entityUuid') (km ^. entityCol)

-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
deleteEntityReferenceFromParentNode ::
     (HasUuid' parentEntity, HasParentUuid' event, HasEntityUuid' event)
  => event
  -> Lens' KnowledgeModel (M.Map U.UUID parentEntity)
  -> Lens' parentEntity [U.UUID]
  -> KnowledgeModel
  -> Either AppError KnowledgeModel
deleteEntityReferenceFromParentNode event parentCollectionInEntities parentCollectionOfChildUuids km =
  case M.lookup (event ^. parentUuid') (km ^. parentCollectionInEntities) of
    Nothing -> Right km
    Just parentNode -> Right . removeEntityReference $ parentNode
  where
    removeEntityReference parentNode =
      km &
      parentCollectionInEntities .~
      M.insert
        (parentNode ^. uuid')
        (parentNode &
         parentCollectionOfChildUuids .~ L.delete (event ^. entityUuid') (parentNode ^. parentCollectionOfChildUuids))
        (km ^. parentCollectionInEntities)
