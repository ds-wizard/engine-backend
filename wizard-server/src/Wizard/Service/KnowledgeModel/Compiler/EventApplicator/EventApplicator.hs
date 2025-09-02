module Wizard.Service.KnowledgeModel.Compiler.EventApplicator.EventApplicator where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
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
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

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
