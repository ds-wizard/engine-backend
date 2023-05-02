module WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEventLenses where

import Shared.Common.Model.Common.Lens
import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent

instance HasUuid' AddAnswerEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' EditAnswerEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

instance HasUuid' DeleteAnswerEvent where
  getUuid entity = entity.uuid
  setUuid entity newValue = entity {uuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasParentUuid' AddAnswerEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' EditAnswerEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

instance HasParentUuid' DeleteAnswerEvent where
  getParentUuid entity = entity.parentUuid
  setParentUuid entity newValue = entity {parentUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasEntityUuid' AddAnswerEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' EditAnswerEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

instance HasEntityUuid' DeleteAnswerEvent where
  getEntityUuid entity = entity.entityUuid
  setEntityUuid entity newValue = entity {entityUuid = newValue}

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
instance HasCreatedAt' AddAnswerEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' EditAnswerEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}

instance HasCreatedAt' DeleteAnswerEvent where
  getCreatedAt entity = entity.createdAt
  setCreatedAt entity newValue = entity {createdAt = newValue}
