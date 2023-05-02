module Wizard.Service.KnowledgeModel.Compilator.Modifier.Move where

import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.EventLenses
import WizardLib.KnowledgeModel.Model.Event.Move.MoveEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses ()

instance EditEntity MoveQuestionEvent Chapter where
  editEntity event = addReferenceToNewParent . deleteReferenceFromOldParent
    where
      deleteReferenceFromOldParent entity =
        if event.parentUuid == entity.uuid
          then entity {questionUuids = filter (event.entityUuid /=) entity.questionUuids}
          else entity
      addReferenceToNewParent entity =
        if event.targetUuid == entity.uuid
          then entity {questionUuids = entity.questionUuids ++ [event.entityUuid]}
          else entity

-- -------------------------------------------------------------------------------------------------------------
instance EditEntity MoveQuestionEvent Question where
  editEntity event = addReferenceToNewParent . deleteReferenceFromOldParent
    where
      deleteReferenceFromOldParent entity =
        if event.parentUuid == getUuid entity
          then setItemTemplateQuestionUuids entity (filter (event.entityUuid /=) $ getItemTemplateQuestionUuids entity)
          else entity
      addReferenceToNewParent entity =
        if event.targetUuid == getUuid entity
          then setItemTemplateQuestionUuids entity (getItemTemplateQuestionUuids entity ++ [event.entityUuid])
          else entity

-- -------------------------------------------------------------------------------------------------------------
instance EditEntity MoveQuestionEvent Answer where
  editEntity event = addReferenceToNewParent . deleteReferenceFromOldParent
    where
      deleteReferenceFromOldParent entity =
        if event.parentUuid == entity.uuid
          then entity {followUpUuids = filter (event.entityUuid /=) entity.followUpUuids}
          else entity
      addReferenceToNewParent entity =
        if event.targetUuid == entity.uuid
          then entity {followUpUuids = entity.followUpUuids ++ [event.entityUuid]}
          else entity

-- -------------------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------------------
instance EditEntity MoveAnswerEvent Question where
  editEntity event = addReferenceToNewParent . deleteReferenceFromOldParent
    where
      deleteReferenceFromOldParent entity =
        if event.parentUuid == getUuid entity
          then setAnswerUuids entity (filter (event.entityUuid /=) $ getAnswerUuids entity)
          else entity
      addReferenceToNewParent entity =
        if event.targetUuid == getUuid entity
          then setAnswerUuids entity (getAnswerUuids entity ++ [event.entityUuid])
          else entity

-- -------------------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------------------
instance EditEntity MoveChoiceEvent Question where
  editEntity event = addReferenceToNewParent . deleteReferenceFromOldParent
    where
      deleteReferenceFromOldParent entity =
        if event.parentUuid == getUuid entity
          then setChoiceUuids entity (filter (event.entityUuid /=) $ getChoiceUuids entity)
          else entity
      addReferenceToNewParent entity =
        if event.targetUuid == getUuid entity
          then setChoiceUuids entity (getChoiceUuids entity ++ [event.entityUuid])
          else entity

-- -------------------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------------------
instance EditEntity MoveExpertEvent Question where
  editEntity event = addReferenceToNewParent . deleteReferenceFromOldParent
    where
      deleteReferenceFromOldParent entity =
        if event.parentUuid == getUuid entity
          then setExpertUuids entity (filter (event.entityUuid /=) $ getExpertUuids entity)
          else entity
      addReferenceToNewParent entity =
        if event.targetUuid == getUuid entity
          then setExpertUuids entity (getExpertUuids entity ++ [event.entityUuid])
          else entity

-- -------------------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------------------
instance EditEntity MoveReferenceEvent Question where
  editEntity event = addReferenceToNewParent . deleteReferenceFromOldParent
    where
      deleteReferenceFromOldParent entity =
        if event.parentUuid == getUuid entity
          then setReferenceUuids entity (filter (event.entityUuid /=) $ getReferenceUuids entity)
          else entity
      addReferenceToNewParent entity =
        if event.targetUuid == getUuid entity
          then setReferenceUuids entity (getReferenceUuids entity ++ [event.entityUuid])
          else entity
