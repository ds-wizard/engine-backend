module Wizard.Service.KnowledgeModel.Compilator.Modifier.Move where

import Control.Lens (Lens', (&), (.~), (^.))
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.EventAccessors
import Shared.Model.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance EditEntity MoveQuestionEvent Chapter where
  editEntity = moveEntity uuid questionUuids

instance EditEntity MoveQuestionEvent Question where
  editEntity = moveEntity uuid' itemTemplateQuestionUuids'

instance EditEntity MoveQuestionEvent Answer where
  editEntity = moveEntity uuid' followUpUuids

-- -------------------------------------------------------------------------------------------------------------
instance EditEntity MoveAnswerEvent Question where
  editEntity = moveEntity uuid' answerUuids'

-- -------------------------------------------------------------------------------------------------------------
instance EditEntity MoveExpertEvent Question where
  editEntity = moveEntity uuid' expertUuids'

-- -------------------------------------------------------------------------------------------------------------
instance EditEntity MoveReferenceEvent Question where
  editEntity = moveEntity uuid' referenceUuids'

-- -------------------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------------------
moveEntity ::
     (EventAccesors event, HasTargetUuid event U.UUID)
  => Lens' entity U.UUID
  -> Lens' entity [U.UUID]
  -> event
  -> entity
  -> entity
moveEntity entityUuid parentUuidList event = addReferenceToNewParent . deleteReferenceFromOldParent
  where
    deleteReferenceFromOldParent entity =
      if getEventParentUuid event == entity ^. entityUuid
        then entity & parentUuidList .~ filter (getEventNodeUuid event /=) (entity ^. parentUuidList)
        else entity
    addReferenceToNewParent entity =
      if event ^. targetUuid == entity ^. entityUuid
        then entity & parentUuidList .~ ((entity ^. parentUuidList) ++ [getEventNodeUuid event])
        else entity
