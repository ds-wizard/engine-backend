module WizardLib.KnowledgeModel.Model.Event.Question.QuestionEventUtil where

import WizardLib.KnowledgeModel.Model.Event.Common.CommonUtil
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent

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
      , isChangedValue event.props
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
