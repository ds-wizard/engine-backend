module Wizard.Service.KnowledgeModel.Squash.Event.Question where

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Wizard.Service.KnowledgeModel.Squash.Event.Common

instance SimpleEventSquash EditQuestionEvent where
  isSimpleEventSquashApplicable (EditOptionsQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.answerUuids) event
  isSimpleEventSquashApplicable (EditMultiChoiceQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.choiceUuids) event
  isSimpleEventSquashApplicable (EditListQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.itemTemplateQuestionUuids) event
  isSimpleEventSquashApplicable (EditValueQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
  isSimpleEventSquashApplicable (EditIntegrationQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.integrationUuid) event
  isSimpleEventSquashApplicable (EditItemSelectQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event
        || isChanged (.listQuestionUuid) event
  isSimpleEventSquashApplicable (EditFileQuestionEvent' event) =
    not $
      isChanged (.requiredPhaseUuid) event
        || isChanged (.tagUuids) event
        || isChanged (.expertUuids) event
        || isChanged (.referenceUuids) event

  --  --------------------------------------
  isReorderEventSquashApplicable (previousEvent, _) (newEvent, _) = previousEvent.entityUuid == newEvent.entityUuid

  --  --------------------------------------
  isTypeChanged (EditOptionsQuestionEvent' oldEvent) (EditOptionsQuestionEvent' newEvent) = False
  isTypeChanged (EditMultiChoiceQuestionEvent' oldEvent) (EditMultiChoiceQuestionEvent' newEvent) = False
  isTypeChanged (EditListQuestionEvent' oldEvent) (EditListQuestionEvent' newEvent) = False
  isTypeChanged (EditValueQuestionEvent' oldEvent) (EditValueQuestionEvent' newEvent) = False
  isTypeChanged (EditIntegrationQuestionEvent' oldEvent) (EditIntegrationQuestionEvent' newEvent) = False
  isTypeChanged (EditItemSelectQuestionEvent' oldEvent) (EditItemSelectQuestionEvent' newEvent) = False
  isTypeChanged (EditFileQuestionEvent' oldEvent) (EditFileQuestionEvent' newEvent) = False
  isTypeChanged _ _ = True

  --  --------------------------------------
  simpleSquashEvent mPreviousEvent (oldEvent, EditOptionsQuestionEvent' oldContent) (newEvent, EditOptionsQuestionEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditQuestionEvent' $
        EditOptionsQuestionEvent' $
          EditOptionsQuestionEvent
            { title = applyValue oldContent newContent (.title)
            , text = applyValue oldContent newContent (.text)
            , requiredPhaseUuid = applyValue oldContent newContent (.requiredPhaseUuid)
            , annotations = applyValue oldContent newContent (.annotations)
            , tagUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.tagUuids)
            , expertUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.expertUuids)
            , referenceUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.referenceUuids)
            , answerUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.answerUuids)
            }
  simpleSquashEvent mPreviousEvent (oldEvent, EditMultiChoiceQuestionEvent' oldContent) (newEvent, EditMultiChoiceQuestionEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditQuestionEvent' $
        EditMultiChoiceQuestionEvent' $
          EditMultiChoiceQuestionEvent
            { title = applyValue oldContent newContent (.title)
            , text = applyValue oldContent newContent (.text)
            , requiredPhaseUuid = applyValue oldContent newContent (.requiredPhaseUuid)
            , annotations = applyValue oldContent newContent (.annotations)
            , tagUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.tagUuids)
            , expertUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.expertUuids)
            , referenceUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.referenceUuids)
            , choiceUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.choiceUuids)
            }
  simpleSquashEvent mPreviousEvent (oldEvent, EditListQuestionEvent' oldContent) (newEvent, EditListQuestionEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditQuestionEvent' $
        EditListQuestionEvent' $
          EditListQuestionEvent
            { title = applyValue oldContent newContent (.title)
            , text = applyValue oldContent newContent (.text)
            , requiredPhaseUuid = applyValue oldContent newContent (.requiredPhaseUuid)
            , annotations = applyValue oldContent newContent (.annotations)
            , tagUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.tagUuids)
            , expertUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.expertUuids)
            , referenceUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.referenceUuids)
            , itemTemplateQuestionUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.itemTemplateQuestionUuids)
            }
  simpleSquashEvent mPreviousEvent (oldEvent, EditValueQuestionEvent' oldContent) (newEvent, EditValueQuestionEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditQuestionEvent' $
        EditValueQuestionEvent' $
          EditValueQuestionEvent
            { title = applyValue oldContent newContent (.title)
            , text = applyValue oldContent newContent (.text)
            , requiredPhaseUuid = applyValue oldContent newContent (.requiredPhaseUuid)
            , annotations = applyValue oldContent newContent (.annotations)
            , tagUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.tagUuids)
            , expertUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.expertUuids)
            , referenceUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.referenceUuids)
            , valueType = applyValue oldContent newContent (.valueType)
            , validations = applyValue oldContent newContent (.validations)
            }
  simpleSquashEvent mPreviousEvent (oldEvent, EditIntegrationQuestionEvent' oldContent) (newEvent, EditIntegrationQuestionEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditQuestionEvent' $
        EditIntegrationQuestionEvent' $
          EditIntegrationQuestionEvent
            { title = applyValue oldContent newContent (.title)
            , text = applyValue oldContent newContent (.text)
            , requiredPhaseUuid = applyValue oldContent newContent (.requiredPhaseUuid)
            , annotations = applyValue oldContent newContent (.annotations)
            , tagUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.tagUuids)
            , expertUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.expertUuids)
            , referenceUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.referenceUuids)
            , integrationUuid = applyValue oldContent newContent (.integrationUuid)
            , variables = applyValue oldContent newContent (.variables)
            }
  simpleSquashEvent mPreviousEvent (oldEvent, EditItemSelectQuestionEvent' oldContent) (newEvent, EditItemSelectQuestionEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditQuestionEvent' $
        EditItemSelectQuestionEvent' $
          EditItemSelectQuestionEvent
            { title = applyValue oldContent newContent (.title)
            , text = applyValue oldContent newContent (.text)
            , requiredPhaseUuid = applyValue oldContent newContent (.requiredPhaseUuid)
            , annotations = applyValue oldContent newContent (.annotations)
            , tagUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.tagUuids)
            , expertUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.expertUuids)
            , referenceUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.referenceUuids)
            , listQuestionUuid = applyValue oldContent newContent (.listQuestionUuid)
            }
  simpleSquashEvent mPreviousEvent (oldEvent, EditFileQuestionEvent' oldContent) (newEvent, EditFileQuestionEvent' newContent) =
    createSquashedEvent oldEvent newEvent $
      EditQuestionEvent' $
        EditFileQuestionEvent' $
          EditFileQuestionEvent
            { title = applyValue oldContent newContent (.title)
            , text = applyValue oldContent newContent (.text)
            , requiredPhaseUuid = applyValue oldContent newContent (.requiredPhaseUuid)
            , annotations = applyValue oldContent newContent (.annotations)
            , tagUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.tagUuids)
            , expertUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.expertUuids)
            , referenceUuids = applyValueIfSameEntity mPreviousEvent (oldEvent, oldContent) (newEvent, newContent) (.referenceUuids)
            , maxSize = applyValue oldContent newContent (.maxSize)
            , fileTypes = applyValue oldContent newContent (.fileTypes)
            }
  simpleSquashEvent previousEvent oldEvent newEvent = error $ "Simple squash event is not applicable for " <> show (oldEvent, newEvent) <> " in " <> show previousEvent
