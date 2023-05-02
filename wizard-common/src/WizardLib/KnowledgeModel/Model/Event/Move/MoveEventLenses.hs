module WizardLib.KnowledgeModel.Model.Event.Move.MoveEventLenses where

import Shared.Common.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Move.MoveEvent

instance HasUuid' MoveQuestionEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' MoveAnswerEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' MoveChoiceEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' MoveExpertEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' MoveReferenceEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' MoveQuestionEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' MoveAnswerEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' MoveChoiceEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' MoveExpertEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' MoveReferenceEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' MoveQuestionEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' MoveAnswerEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' MoveChoiceEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' MoveExpertEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' MoveReferenceEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' MoveQuestionEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' MoveAnswerEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' MoveChoiceEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' MoveExpertEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' MoveReferenceEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasTargetUuid' MoveQuestionEvent where
  getTargetUuid entity = entity.targetUuid
  setTargetUuid entity newValue = entity {targetUuid = newValue}

instance HasTargetUuid' MoveAnswerEvent where
  getTargetUuid entity = entity.targetUuid
  setTargetUuid entity newValue = entity {targetUuid = newValue}

instance HasTargetUuid' MoveChoiceEvent where
  getTargetUuid entity = entity.targetUuid
  setTargetUuid entity newValue = entity {targetUuid = newValue}

instance HasTargetUuid' MoveExpertEvent where
  getTargetUuid entity = entity.targetUuid
  setTargetUuid entity newValue = entity {targetUuid = newValue}

instance HasTargetUuid' MoveReferenceEvent where
  getTargetUuid entity = entity.targetUuid
  setTargetUuid entity newValue = entity {targetUuid = newValue}
