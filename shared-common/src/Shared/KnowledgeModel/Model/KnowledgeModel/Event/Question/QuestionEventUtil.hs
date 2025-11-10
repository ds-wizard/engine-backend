module Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEventUtil where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Common.CommonUtil
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent

instance IsEmptyEvent EditQuestionEvent where
  isEmptyEvent (EditOptionsQuestionEvent' event) = isEmptyEvent event
  isEmptyEvent (EditMultiChoiceQuestionEvent' event) = isEmptyEvent event
  isEmptyEvent (EditListQuestionEvent' event) = isEmptyEvent event
  isEmptyEvent (EditValueQuestionEvent' event) = isEmptyEvent event
  isEmptyEvent (EditIntegrationQuestionEvent' event) = isEmptyEvent event
  isEmptyEvent (EditItemSelectQuestionEvent' event) = isEmptyEvent event
  isEmptyEvent (EditFileQuestionEvent' event) = isEmptyEvent event

instance IsEmptyEvent EditOptionsQuestionEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.requiredPhaseUuid
      , isChangedValue event.annotations
      , isChangedValue event.tagUuids
      , isChangedValue event.expertUuids
      , isChangedValue event.referenceUuids
      , isChangedValue event.answerUuids
      ]

instance IsEmptyEvent EditMultiChoiceQuestionEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.requiredPhaseUuid
      , isChangedValue event.annotations
      , isChangedValue event.tagUuids
      , isChangedValue event.expertUuids
      , isChangedValue event.referenceUuids
      , isChangedValue event.choiceUuids
      ]

instance IsEmptyEvent EditListQuestionEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.requiredPhaseUuid
      , isChangedValue event.annotations
      , isChangedValue event.tagUuids
      , isChangedValue event.expertUuids
      , isChangedValue event.referenceUuids
      , isChangedValue event.itemTemplateQuestionUuids
      ]

instance IsEmptyEvent EditValueQuestionEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.requiredPhaseUuid
      , isChangedValue event.annotations
      , isChangedValue event.tagUuids
      , isChangedValue event.expertUuids
      , isChangedValue event.referenceUuids
      , isChangedValue event.valueType
      , isChangedValue event.validations
      ]

instance IsEmptyEvent EditIntegrationQuestionEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.requiredPhaseUuid
      , isChangedValue event.annotations
      , isChangedValue event.tagUuids
      , isChangedValue event.expertUuids
      , isChangedValue event.referenceUuids
      , isChangedValue event.integrationUuid
      , isChangedValue event.variables
      ]

instance IsEmptyEvent EditItemSelectQuestionEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.requiredPhaseUuid
      , isChangedValue event.annotations
      , isChangedValue event.tagUuids
      , isChangedValue event.expertUuids
      , isChangedValue event.referenceUuids
      , isChangedValue event.listQuestionUuid
      ]

instance IsEmptyEvent EditFileQuestionEvent where
  isEmptyEvent event =
    or
      [ isChangedValue event.title
      , isChangedValue event.text
      , isChangedValue event.requiredPhaseUuid
      , isChangedValue event.annotations
      , isChangedValue event.tagUuids
      , isChangedValue event.expertUuids
      , isChangedValue event.referenceUuids
      , isChangedValue event.maxSize
      , isChangedValue event.fileTypes
      ]
