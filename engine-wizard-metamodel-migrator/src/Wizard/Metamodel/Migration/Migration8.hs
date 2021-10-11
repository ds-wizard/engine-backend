module Wizard.Metamodel.Migration.Migration8 where

import Data.Aeson (FromJSON, Result(..), ToJSON(toJSON), Value, fromJSON)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import qualified Wizard.Metamodel.Event.Version8 as V8
import qualified Wizard.Metamodel.Event.Version8.Answer as V8
import qualified Wizard.Metamodel.Event.Version8.Chapter as V8
import qualified Wizard.Metamodel.Event.Version8.Choice as V8
import qualified Wizard.Metamodel.Event.Version8.Common as V8
import qualified Wizard.Metamodel.Event.Version8.Expert as V8
import qualified Wizard.Metamodel.Event.Version8.Integration as V8
import qualified Wizard.Metamodel.Event.Version8.KnowledgeModel as V8
import qualified Wizard.Metamodel.Event.Version8.Metric as V8
import qualified Wizard.Metamodel.Event.Version8.Phase as V8
import qualified Wizard.Metamodel.Event.Version8.Question as V8
import qualified Wizard.Metamodel.Event.Version8.Reference as V8
import qualified Wizard.Metamodel.Event.Version8.Tag as V8
import qualified Wizard.Metamodel.Event.Version9 as V9
import qualified Wizard.Metamodel.Event.Version9.Answer as V9
import qualified Wizard.Metamodel.Event.Version9.Chapter as V9
import qualified Wizard.Metamodel.Event.Version9.Choice as V9
import qualified Wizard.Metamodel.Event.Version9.Common as V9
import qualified Wizard.Metamodel.Event.Version9.Expert as V9
import qualified Wizard.Metamodel.Event.Version9.Integration as V9
import qualified Wizard.Metamodel.Event.Version9.KnowledgeModel as V9
import qualified Wizard.Metamodel.Event.Version9.Metric as V9
import qualified Wizard.Metamodel.Event.Version9.Phase as V9
import qualified Wizard.Metamodel.Event.Version9.Question as V9
import qualified Wizard.Metamodel.Event.Version9.Reference as V9
import qualified Wizard.Metamodel.Event.Version9.Tag as V9

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

defaultUpgrade :: (FromJSON t, ToJSON f) => f -> Either String t
defaultUpgrade = result2Either . fromJSON . toJSON

defaultAdditional :: f -> t -> Either String [Value]
defaultAdditional _ _ = Right []

class (FromJSON t, ToJSON f) =>
      Upgradeable f t
  where
  upgrade :: f -> Either String t
  upgrade = defaultUpgrade

instance (Upgradeable f t) => Upgradeable [f] [t] where
  upgrade lst =
    case partitionEithers $ map upgrade lst of
      ([], r) -> Right r
      (x:_, _) -> Left x

instance Upgradeable Char Char where
  upgrade = Right

instance Upgradeable U.UUID U.UUID where
  upgrade = Right

instance Upgradeable (Maybe Int) (Maybe Int) where
  upgrade = Right

instance Upgradeable (Maybe String) (Maybe String) where
  upgrade = Right

instance Upgradeable (Maybe U.UUID) (Maybe U.UUID) where
  upgrade = Right

instance Upgradeable (M.Map String String) (M.Map String String) where
  upgrade = Right

instance Upgradeable String (Maybe String) where
  upgrade = Right . Just

instance (Upgradeable f t) => Upgradeable (V8.EventField f) (V9.EventField t) where
  upgrade V8.NothingChanged = Right V9.NothingChanged
  upgrade (V8.ChangedValue x) = V9.ChangedValue <$> upgrade x

instance Upgradeable V8.QuestionValueType V9.QuestionValueType where
  upgrade V8.StringQuestionValueType = Right V9.StringQuestionValueType
  upgrade V8.NumberQuestionValueType = Right V9.NumberQuestionValueType
  upgrade V8.DateQuestionValueType = Right V9.DateQuestionValueType
  upgrade V8.TextQuestionValueType = Right V9.TextQuestionValueType

instance Upgradeable V8.MetricMeasure V9.MetricMeasure

instance Upgradeable V8.AddKnowledgeModelEvent V9.AddKnowledgeModelEvent where
  upgrade V8.AddKnowledgeModelEvent {..} =
    Right $
    V9.AddKnowledgeModelEvent
      _addKnowledgeModelEventUuid
      _addKnowledgeModelEventParentUuid
      _addKnowledgeModelEventEntityUuid
      M.empty

instance Upgradeable V8.EditKnowledgeModelEvent V9.EditKnowledgeModelEvent where
  upgrade V8.EditKnowledgeModelEvent {..} =
    V9.EditKnowledgeModelEvent
      _editKnowledgeModelEventUuid
      _editKnowledgeModelEventParentUuid
      _editKnowledgeModelEventEntityUuid <$>
    Right V9.NothingChanged <*>
    upgrade _editKnowledgeModelEventChapterUuids <*>
    upgrade _editKnowledgeModelEventTagUuids <*>
    upgrade _editKnowledgeModelEventIntegrationUuids <*>
    upgrade _editKnowledgeModelEventMetricUuids <*>
    upgrade _editKnowledgeModelEventPhaseUuids

instance Upgradeable V8.AddChapterEvent V9.AddChapterEvent where
  upgrade V8.AddChapterEvent {..} =
    Right $
    V9.AddChapterEvent
      _addChapterEventUuid
      _addChapterEventParentUuid
      _addChapterEventEntityUuid
      _addChapterEventTitle
      _addChapterEventText
      M.empty

instance Upgradeable V8.EditChapterEvent V9.EditChapterEvent where
  upgrade V8.EditChapterEvent {..} =
    V9.EditChapterEvent _editChapterEventUuid _editChapterEventParentUuid _editChapterEventEntityUuid <$>
    upgrade _editChapterEventTitle <*>
    upgrade _editChapterEventText <*>
    Right V9.NothingChanged <*>
    upgrade _editChapterEventQuestionUuids

instance Upgradeable V8.AddQuestionEvent V9.AddQuestionEvent where
  upgrade (V8.AddOptionsQuestionEvent' event) = V9.AddOptionsQuestionEvent' <$> upgrade event
  upgrade (V8.AddMultiChoiceQuestionEvent' event) = V9.AddMultiChoiceQuestionEvent' <$> upgrade event
  upgrade (V8.AddListQuestionEvent' event) = V9.AddListQuestionEvent' <$> upgrade event
  upgrade (V8.AddValueQuestionEvent' event) = V9.AddValueQuestionEvent' <$> upgrade event
  upgrade (V8.AddIntegrationQuestionEvent' event) = V9.AddIntegrationQuestionEvent' <$> upgrade event

instance Upgradeable V8.AddOptionsQuestionEvent V9.AddOptionsQuestionEvent where
  upgrade V8.AddOptionsQuestionEvent {..} =
    Right $
    V9.AddOptionsQuestionEvent
      _addOptionsQuestionEventUuid
      _addOptionsQuestionEventParentUuid
      _addOptionsQuestionEventEntityUuid
      _addOptionsQuestionEventTitle
      _addOptionsQuestionEventText
      _addOptionsQuestionEventRequiredPhaseUuid
      M.empty
      _addOptionsQuestionEventTagUuids

instance Upgradeable V8.AddMultiChoiceQuestionEvent V9.AddMultiChoiceQuestionEvent where
  upgrade V8.AddMultiChoiceQuestionEvent {..} =
    Right $
    V9.AddMultiChoiceQuestionEvent
      _addMultiChoiceQuestionEventUuid
      _addMultiChoiceQuestionEventParentUuid
      _addMultiChoiceQuestionEventEntityUuid
      _addMultiChoiceQuestionEventTitle
      _addMultiChoiceQuestionEventText
      _addMultiChoiceQuestionEventRequiredPhaseUuid
      M.empty
      _addMultiChoiceQuestionEventTagUuids

instance Upgradeable V8.AddListQuestionEvent V9.AddListQuestionEvent where
  upgrade V8.AddListQuestionEvent {..} =
    Right $
    V9.AddListQuestionEvent
      _addListQuestionEventUuid
      _addListQuestionEventParentUuid
      _addListQuestionEventEntityUuid
      _addListQuestionEventTitle
      _addListQuestionEventText
      _addListQuestionEventRequiredPhaseUuid
      M.empty
      _addListQuestionEventTagUuids

instance Upgradeable V8.AddValueQuestionEvent V9.AddValueQuestionEvent where
  upgrade V8.AddValueQuestionEvent {..} =
    V9.AddValueQuestionEvent
      _addValueQuestionEventUuid
      _addValueQuestionEventParentUuid
      _addValueQuestionEventEntityUuid
      _addValueQuestionEventTitle
      _addValueQuestionEventText
      _addValueQuestionEventRequiredPhaseUuid
      M.empty
      _addValueQuestionEventTagUuids <$>
    upgrade _addValueQuestionEventValueType

instance Upgradeable V8.AddIntegrationQuestionEvent V9.AddIntegrationQuestionEvent where
  upgrade V8.AddIntegrationQuestionEvent {..} =
    Right $
    V9.AddIntegrationQuestionEvent
      _addIntegrationQuestionEventUuid
      _addIntegrationQuestionEventParentUuid
      _addIntegrationQuestionEventEntityUuid
      _addIntegrationQuestionEventTitle
      _addIntegrationQuestionEventText
      _addIntegrationQuestionEventRequiredPhaseUuid
      M.empty
      _addIntegrationQuestionEventTagUuids
      _addIntegrationQuestionEventIntegrationUuid
      _addIntegrationQuestionEventProps

instance Upgradeable V8.EditQuestionEvent V9.EditQuestionEvent where
  upgrade (V8.EditOptionsQuestionEvent' event) = V9.EditOptionsQuestionEvent' <$> upgrade event
  upgrade (V8.EditMultiChoiceQuestionEvent' event) = V9.EditMultiChoiceQuestionEvent' <$> upgrade event
  upgrade (V8.EditListQuestionEvent' event) = V9.EditListQuestionEvent' <$> upgrade event
  upgrade (V8.EditValueQuestionEvent' event) = V9.EditValueQuestionEvent' <$> upgrade event
  upgrade (V8.EditIntegrationQuestionEvent' event) = V9.EditIntegrationQuestionEvent' <$> upgrade event

instance Upgradeable V8.EditOptionsQuestionEvent V9.EditOptionsQuestionEvent where
  upgrade V8.EditOptionsQuestionEvent {..} =
    V9.EditOptionsQuestionEvent
      _editOptionsQuestionEventUuid
      _editOptionsQuestionEventParentUuid
      _editOptionsQuestionEventEntityUuid <$>
    upgrade _editOptionsQuestionEventTitle <*>
    upgrade _editOptionsQuestionEventText <*>
    upgrade _editOptionsQuestionEventRequiredPhaseUuid <*>
    Right V9.NothingChanged <*>
    upgrade _editOptionsQuestionEventTagUuids <*>
    upgrade _editOptionsQuestionEventExpertUuids <*>
    upgrade _editOptionsQuestionEventReferenceUuids <*>
    upgrade _editOptionsQuestionEventAnswerUuids

instance Upgradeable V8.EditMultiChoiceQuestionEvent V9.EditMultiChoiceQuestionEvent where
  upgrade V8.EditMultiChoiceQuestionEvent {..} =
    V9.EditMultiChoiceQuestionEvent
      _editMultiChoiceQuestionEventUuid
      _editMultiChoiceQuestionEventParentUuid
      _editMultiChoiceQuestionEventEntityUuid <$>
    upgrade _editMultiChoiceQuestionEventTitle <*>
    upgrade _editMultiChoiceQuestionEventText <*>
    upgrade _editMultiChoiceQuestionEventRequiredPhaseUuid <*>
    Right V9.NothingChanged <*>
    upgrade _editMultiChoiceQuestionEventTagUuids <*>
    upgrade _editMultiChoiceQuestionEventExpertUuids <*>
    upgrade _editMultiChoiceQuestionEventReferenceUuids <*>
    upgrade _editMultiChoiceQuestionEventChoiceUuids

instance Upgradeable V8.EditListQuestionEvent V9.EditListQuestionEvent where
  upgrade V8.EditListQuestionEvent {..} =
    V9.EditListQuestionEvent
      _editListQuestionEventUuid
      _editListQuestionEventParentUuid
      _editListQuestionEventEntityUuid <$>
    upgrade _editListQuestionEventTitle <*>
    upgrade _editListQuestionEventText <*>
    upgrade _editListQuestionEventRequiredPhaseUuid <*>
    Right V9.NothingChanged <*>
    upgrade _editListQuestionEventTagUuids <*>
    upgrade _editListQuestionEventExpertUuids <*>
    upgrade _editListQuestionEventReferenceUuids <*>
    upgrade _editListQuestionEventItemTemplateQuestionUuids

instance Upgradeable V8.EditValueQuestionEvent V9.EditValueQuestionEvent where
  upgrade V8.EditValueQuestionEvent {..} =
    V9.EditValueQuestionEvent
      _editValueQuestionEventUuid
      _editValueQuestionEventParentUuid
      _editValueQuestionEventEntityUuid <$>
    upgrade _editValueQuestionEventTitle <*>
    upgrade _editValueQuestionEventText <*>
    upgrade _editValueQuestionEventRequiredPhaseUuid <*>
    Right V9.NothingChanged <*>
    upgrade _editValueQuestionEventTagUuids <*>
    upgrade _editValueQuestionEventExpertUuids <*>
    upgrade _editValueQuestionEventReferenceUuids <*>
    upgrade _editValueQuestionEventValueType

instance Upgradeable V8.EditIntegrationQuestionEvent V9.EditIntegrationQuestionEvent where
  upgrade V8.EditIntegrationQuestionEvent {..} =
    V9.EditIntegrationQuestionEvent
      _editIntegrationQuestionEventUuid
      _editIntegrationQuestionEventParentUuid
      _editIntegrationQuestionEventEntityUuid <$>
    upgrade _editIntegrationQuestionEventTitle <*>
    upgrade _editIntegrationQuestionEventText <*>
    upgrade _editIntegrationQuestionEventRequiredPhaseUuid <*>
    Right V9.NothingChanged <*>
    upgrade _editIntegrationQuestionEventTagUuids <*>
    upgrade _editIntegrationQuestionEventExpertUuids <*>
    upgrade _editIntegrationQuestionEventReferenceUuids <*>
    upgrade _editIntegrationQuestionEventIntegrationUuid <*>
    upgrade _editIntegrationQuestionEventProps

instance Upgradeable V8.AddAnswerEvent V9.AddAnswerEvent where
  upgrade V8.AddAnswerEvent {..} =
    V9.AddAnswerEvent
      _addAnswerEventUuid
      _addAnswerEventParentUuid
      _addAnswerEventEntityUuid
      _addAnswerEventLabel
      _addAnswerEventAdvice
      M.empty <$>
    upgrade _addAnswerEventMetricMeasures

instance Upgradeable V8.EditAnswerEvent V9.EditAnswerEvent where
  upgrade V8.EditAnswerEvent {..} =
    V9.EditAnswerEvent _editAnswerEventUuid _editAnswerEventParentUuid _editAnswerEventEntityUuid <$>
    upgrade _editAnswerEventLabel <*>
    upgrade _editAnswerEventAdvice <*>
    Right V9.NothingChanged <*>
    upgrade _editAnswerEventFollowUpUuids <*>
    upgrade _editAnswerEventMetricMeasures

instance Upgradeable V8.AddChoiceEvent V9.AddChoiceEvent where
  upgrade V8.AddChoiceEvent {..} =
    Right $
    V9.AddChoiceEvent
      _addChoiceEventUuid
      _addChoiceEventParentUuid
      _addChoiceEventEntityUuid
      _addChoiceEventLabel
      M.empty

instance Upgradeable V8.EditChoiceEvent V9.EditChoiceEvent where
  upgrade V8.EditChoiceEvent {..} =
    V9.EditChoiceEvent _editChoiceEventUuid _editChoiceEventParentUuid _editChoiceEventEntityUuid <$>
    upgrade _editChoiceEventLabel <*>
    Right V9.NothingChanged

instance Upgradeable V8.AddExpertEvent V9.AddExpertEvent where
  upgrade V8.AddExpertEvent {..} =
    Right $
    V9.AddExpertEvent
      _addExpertEventUuid
      _addExpertEventParentUuid
      _addExpertEventEntityUuid
      _addExpertEventName
      _addExpertEventEmail
      M.empty

instance Upgradeable V8.EditExpertEvent V9.EditExpertEvent where
  upgrade V8.EditExpertEvent {..} =
    V9.EditExpertEvent _editExpertEventUuid _editExpertEventParentUuid _editExpertEventEntityUuid <$>
    upgrade _editExpertEventName <*>
    upgrade _editExpertEventEmail <*>
    Right V9.NothingChanged

instance Upgradeable V8.AddReferenceEvent V9.AddReferenceEvent where
  upgrade (V8.AddResourcePageReferenceEvent' event) = V9.AddResourcePageReferenceEvent' <$> upgrade event
  upgrade (V8.AddURLReferenceEvent' event) = V9.AddURLReferenceEvent' <$> upgrade event
  upgrade (V8.AddCrossReferenceEvent' event) = V9.AddCrossReferenceEvent' <$> upgrade event

instance Upgradeable V8.AddResourcePageReferenceEvent V9.AddResourcePageReferenceEvent where
  upgrade V8.AddResourcePageReferenceEvent {..} =
    Right $
    V9.AddResourcePageReferenceEvent
      _addResourcePageReferenceEventUuid
      _addResourcePageReferenceEventParentUuid
      _addResourcePageReferenceEventEntityUuid
      _addResourcePageReferenceEventShortUuid
      M.empty

instance Upgradeable V8.AddURLReferenceEvent V9.AddURLReferenceEvent where
  upgrade V8.AddURLReferenceEvent {..} =
    Right $
    V9.AddURLReferenceEvent
      _addURLReferenceEventUuid
      _addURLReferenceEventParentUuid
      _addURLReferenceEventEntityUuid
      _addURLReferenceEventUrl
      _addURLReferenceEventLabel
      M.empty

instance Upgradeable V8.AddCrossReferenceEvent V9.AddCrossReferenceEvent where
  upgrade V8.AddCrossReferenceEvent {..} =
    Right $
    V9.AddCrossReferenceEvent
      _addCrossReferenceEventUuid
      _addCrossReferenceEventParentUuid
      _addCrossReferenceEventEntityUuid
      _addCrossReferenceEventTargetUuid
      _addCrossReferenceEventDescription
      M.empty

instance Upgradeable V8.EditReferenceEvent V9.EditReferenceEvent where
  upgrade (V8.EditResourcePageReferenceEvent' event) = V9.EditResourcePageReferenceEvent' <$> upgrade event
  upgrade (V8.EditURLReferenceEvent' event) = V9.EditURLReferenceEvent' <$> upgrade event
  upgrade (V8.EditCrossReferenceEvent' event) = V9.EditCrossReferenceEvent' <$> upgrade event

instance Upgradeable V8.EditResourcePageReferenceEvent V9.EditResourcePageReferenceEvent where
  upgrade V8.EditResourcePageReferenceEvent {..} =
    V9.EditResourcePageReferenceEvent
      _editResourcePageReferenceEventUuid
      _editResourcePageReferenceEventParentUuid
      _editResourcePageReferenceEventEntityUuid <$>
    upgrade _editResourcePageReferenceEventShortUuid <*>
    Right V9.NothingChanged

instance Upgradeable V8.EditURLReferenceEvent V9.EditURLReferenceEvent where
  upgrade V8.EditURLReferenceEvent {..} =
    V9.EditURLReferenceEvent
      _editURLReferenceEventUuid
      _editURLReferenceEventParentUuid
      _editURLReferenceEventEntityUuid <$>
    upgrade _editURLReferenceEventUrl <*>
    upgrade _editURLReferenceEventLabel <*>
    Right V9.NothingChanged

instance Upgradeable V8.EditCrossReferenceEvent V9.EditCrossReferenceEvent where
  upgrade V8.EditCrossReferenceEvent {..} =
    V9.EditCrossReferenceEvent
      _editCrossReferenceEventUuid
      _editCrossReferenceEventParentUuid
      _editCrossReferenceEventEntityUuid <$>
    upgrade _editCrossReferenceEventTargetUuid <*>
    upgrade _editCrossReferenceEventDescription <*>
    Right V9.NothingChanged

instance Upgradeable V8.AddTagEvent V9.AddTagEvent where
  upgrade V8.AddTagEvent {..} =
    Right $
    V9.AddTagEvent
      _addTagEventUuid
      _addTagEventParentUuid
      _addTagEventEntityUuid
      _addTagEventName
      _addTagEventDescription
      _addTagEventColor
      M.empty

instance Upgradeable V8.EditTagEvent V9.EditTagEvent where
  upgrade V8.EditTagEvent {..} =
    V9.EditTagEvent _editTagEventUuid _editTagEventParentUuid _editTagEventEntityUuid <$> upgrade _editTagEventName <*>
    upgrade _editTagEventDescription <*>
    upgrade _editTagEventColor <*>
    Right V9.NothingChanged

instance Upgradeable V8.AddIntegrationEvent V9.AddIntegrationEvent where
  upgrade V8.AddIntegrationEvent {..} =
    Right $
    V9.AddIntegrationEvent
      _addIntegrationEventUuid
      _addIntegrationEventParentUuid
      _addIntegrationEventEntityUuid
      _addIntegrationEventIId
      _addIntegrationEventName
      _addIntegrationEventProps
      _addIntegrationEventLogo
      _addIntegrationEventRequestMethod
      _addIntegrationEventRequestUrl
      _addIntegrationEventRequestHeaders
      _addIntegrationEventRequestBody
      _addIntegrationEventResponseListField
      _addIntegrationEventResponseIdField
      _addIntegrationEventResponseNameField
      _addIntegrationEventItemUrl
      M.empty

instance Upgradeable V8.EditIntegrationEvent V9.EditIntegrationEvent where
  upgrade V8.EditIntegrationEvent {..} =
    V9.EditIntegrationEvent _editIntegrationEventUuid _editIntegrationEventParentUuid _editIntegrationEventEntityUuid <$>
    upgrade _editIntegrationEventIId <*>
    upgrade _editIntegrationEventName <*>
    upgrade _editIntegrationEventProps <*>
    upgrade _editIntegrationEventLogo <*>
    upgrade _editIntegrationEventRequestMethod <*>
    upgrade _editIntegrationEventRequestUrl <*>
    upgrade _editIntegrationEventRequestHeaders <*>
    upgrade _editIntegrationEventRequestBody <*>
    upgrade _editIntegrationEventResponseListField <*>
    upgrade _editIntegrationEventResponseIdField <*>
    upgrade _editIntegrationEventResponseNameField <*>
    upgrade _editIntegrationEventItemUrl <*>
    Right V9.NothingChanged

instance Upgradeable V8.AddMetricEvent V9.AddMetricEvent where
  upgrade V8.AddMetricEvent {..} =
    Right $
    V9.AddMetricEvent
      _addMetricEventUuid
      _addMetricEventParentUuid
      _addMetricEventEntityUuid
      _addMetricEventTitle
      _addMetricEventAbbreviation
      _addMetricEventDescription
      M.empty

instance Upgradeable V8.EditMetricEvent V9.EditMetricEvent where
  upgrade V8.EditMetricEvent {..} =
    V9.EditMetricEvent _editMetricEventUuid _editMetricEventParentUuid _editMetricEventEntityUuid <$>
    upgrade _editMetricEventTitle <*>
    upgrade _editMetricEventAbbreviation <*>
    upgrade _editMetricEventDescription <*>
    Right V9.NothingChanged

instance Upgradeable V8.AddPhaseEvent V9.AddPhaseEvent where
  upgrade V8.AddPhaseEvent {..} =
    Right $
    V9.AddPhaseEvent
      _addPhaseEventUuid
      _addPhaseEventParentUuid
      _addPhaseEventEntityUuid
      _addPhaseEventTitle
      _addPhaseEventDescription
      M.empty

instance Upgradeable V8.EditPhaseEvent V9.EditPhaseEvent where
  upgrade V8.EditPhaseEvent {..} =
    V9.EditPhaseEvent _editPhaseEventUuid _editPhaseEventParentUuid _editPhaseEventEntityUuid <$>
    upgrade _editPhaseEventTitle <*>
    upgrade _editPhaseEventDescription <*>
    Right V9.NothingChanged

instance Upgradeable V8.Event V9.Event where
  upgrade (V8.AddKnowledgeModelEvent' event) = V9.AddKnowledgeModelEvent' <$> upgrade event
  upgrade (V8.EditKnowledgeModelEvent' event) = V9.EditKnowledgeModelEvent' <$> upgrade event
  upgrade (V8.AddChapterEvent' event) = V9.AddChapterEvent' <$> upgrade event
  upgrade (V8.EditChapterEvent' event) = V9.EditChapterEvent' <$> upgrade event
  upgrade (V8.DeleteChapterEvent' event) = V9.DeleteChapterEvent' <$> defaultUpgrade event
  upgrade (V8.AddQuestionEvent' event) = V9.AddQuestionEvent' <$> upgrade event
  upgrade (V8.EditQuestionEvent' event) = V9.EditQuestionEvent' <$> upgrade event
  upgrade (V8.DeleteQuestionEvent' event) = V9.DeleteQuestionEvent' <$> defaultUpgrade event
  upgrade (V8.AddAnswerEvent' event) = V9.AddAnswerEvent' <$> upgrade event
  upgrade (V8.EditAnswerEvent' event) = V9.EditAnswerEvent' <$> upgrade event
  upgrade (V8.DeleteAnswerEvent' event) = V9.DeleteAnswerEvent' <$> defaultUpgrade event
  upgrade (V8.AddChoiceEvent' event) = V9.AddChoiceEvent' <$> upgrade event
  upgrade (V8.EditChoiceEvent' event) = V9.EditChoiceEvent' <$> upgrade event
  upgrade (V8.DeleteChoiceEvent' event) = V9.DeleteChoiceEvent' <$> defaultUpgrade event
  upgrade (V8.AddExpertEvent' event) = V9.AddExpertEvent' <$> upgrade event
  upgrade (V8.EditExpertEvent' event) = V9.EditExpertEvent' <$> upgrade event
  upgrade (V8.DeleteExpertEvent' event) = V9.DeleteExpertEvent' <$> defaultUpgrade event
  upgrade (V8.AddReferenceEvent' event) = V9.AddReferenceEvent' <$> upgrade event
  upgrade (V8.EditReferenceEvent' event) = V9.EditReferenceEvent' <$> upgrade event
  upgrade (V8.DeleteReferenceEvent' event) = V9.DeleteReferenceEvent' <$> defaultUpgrade event
  upgrade (V8.AddTagEvent' event) = V9.AddTagEvent' <$> upgrade event
  upgrade (V8.EditTagEvent' event) = V9.EditTagEvent' <$> upgrade event
  upgrade (V8.DeleteTagEvent' event) = V9.DeleteTagEvent' <$> defaultUpgrade event
  upgrade (V8.AddIntegrationEvent' event) = V9.AddIntegrationEvent' <$> upgrade event
  upgrade (V8.EditIntegrationEvent' event) = V9.EditIntegrationEvent' <$> upgrade event
  upgrade (V8.DeleteIntegrationEvent' event) = V9.DeleteIntegrationEvent' <$> defaultUpgrade event
  upgrade (V8.AddMetricEvent' event) = V9.AddMetricEvent' <$> upgrade event
  upgrade (V8.EditMetricEvent' event) = V9.EditMetricEvent' <$> upgrade event
  upgrade (V8.DeleteMetricEvent' event) = V9.DeleteMetricEvent' <$> defaultUpgrade event
  upgrade (V8.AddPhaseEvent' event) = V9.AddPhaseEvent' <$> upgrade event
  upgrade (V8.EditPhaseEvent' event) = V9.EditPhaseEvent' <$> upgrade event
  upgrade (V8.DeletePhaseEvent' event) = V9.DeletePhaseEvent' <$> defaultUpgrade event
  upgrade (V8.MoveQuestionEvent' event) = V9.MoveQuestionEvent' <$> defaultUpgrade event
  upgrade (V8.MoveAnswerEvent' event) = V9.MoveAnswerEvent' <$> defaultUpgrade event
  upgrade (V8.MoveChoiceEvent' event) = V9.MoveChoiceEvent' <$> defaultUpgrade event
  upgrade (V8.MoveExpertEvent' event) = V9.MoveExpertEvent' <$> defaultUpgrade event
  upgrade (V8.MoveReferenceEvent' event) = V9.MoveReferenceEvent' <$> defaultUpgrade event

migrateEventValue :: Value -> Either String [Value]
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V8.Event)
  return [toJSON (newEvent :: V9.Event)]
