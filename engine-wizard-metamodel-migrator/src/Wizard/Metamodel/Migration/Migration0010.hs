module Wizard.Metamodel.Migration.Migration0010 where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, Result(..), ToJSON(toJSON), Value, fromJSON)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import qualified Wizard.Metamodel.Event.Version0010 as V10
import qualified Wizard.Metamodel.Event.Version0010.Answer as V10
import qualified Wizard.Metamodel.Event.Version0010.Chapter as V10
import qualified Wizard.Metamodel.Event.Version0010.Choice as V10
import qualified Wizard.Metamodel.Event.Version0010.Common as V10
import qualified Wizard.Metamodel.Event.Version0010.Expert as V10
import qualified Wizard.Metamodel.Event.Version0010.Integration as V10
import qualified Wizard.Metamodel.Event.Version0010.KnowledgeModel as V10
import qualified Wizard.Metamodel.Event.Version0010.Metric as V10
import qualified Wizard.Metamodel.Event.Version0010.Move as V10
import qualified Wizard.Metamodel.Event.Version0010.Phase as V10
import qualified Wizard.Metamodel.Event.Version0010.Question as V10
import qualified Wizard.Metamodel.Event.Version0010.Reference as V10
import qualified Wizard.Metamodel.Event.Version0010.Tag as V10
import qualified Wizard.Metamodel.Event.Version0011 as V11
import qualified Wizard.Metamodel.Event.Version0011.Answer as V11
import qualified Wizard.Metamodel.Event.Version0011.Chapter as V11
import qualified Wizard.Metamodel.Event.Version0011.Choice as V11
import qualified Wizard.Metamodel.Event.Version0011.Common as V11
import qualified Wizard.Metamodel.Event.Version0011.Expert as V11
import qualified Wizard.Metamodel.Event.Version0011.Integration as V11
import qualified Wizard.Metamodel.Event.Version0011.KnowledgeModel as V11
import qualified Wizard.Metamodel.Event.Version0011.Metric as V11
import qualified Wizard.Metamodel.Event.Version0011.Move as V11
import qualified Wizard.Metamodel.Event.Version0011.Phase as V11
import qualified Wizard.Metamodel.Event.Version0011.Question as V11
import qualified Wizard.Metamodel.Event.Version0011.Reference as V11
import qualified Wizard.Metamodel.Event.Version0011.Tag as V11
import Wizard.Metamodel.Migration.MigrationContext

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

mapToList :: M.Map String String -> [V11.MapEntry String String]
mapToList = map (uncurry V11.MapEntry) . M.toList

defaultUpgrade :: (FromJSON t, ToJSON f) => MigrationContext -> f -> Either String t
defaultUpgrade ctx = result2Either . fromJSON . toJSON

class (FromJSON t, ToJSON f) =>
      Upgradeable f t
  where
  upgrade :: MigrationContext -> f -> Either String t
  upgrade = defaultUpgrade

instance (Upgradeable f t) => Upgradeable [f] [t] where
  upgrade ctx lst =
    case partitionEithers $ map (upgrade ctx) lst of
      ([], r) -> Right r
      (x:_, _) -> Left x

instance Upgradeable Char Char where
  upgrade ctx = Right

instance Upgradeable U.UUID U.UUID where
  upgrade ctx = Right

instance Upgradeable (Maybe Int) (Maybe Int) where
  upgrade ctx = Right

instance Upgradeable (Maybe String) (Maybe String) where
  upgrade ctx = Right

instance Upgradeable (Maybe U.UUID) (Maybe U.UUID) where
  upgrade ctx = Right

instance Upgradeable (M.Map String String) (M.Map String String) where
  upgrade ctx = Right

instance Upgradeable (M.Map String String) [V11.MapEntry String String] where
  upgrade ctx = Right . mapToList

instance Upgradeable String (Maybe String) where
  upgrade ctx = Right . Just

instance (Upgradeable f t) => Upgradeable (V10.EventField f) (V11.EventField t) where
  upgrade ctx V10.NothingChanged = Right V11.NothingChanged
  upgrade ctx (V10.ChangedValue x) = V11.ChangedValue <$> upgrade ctx x

instance Upgradeable V10.QuestionValueType V11.QuestionValueType where
  upgrade ctx V10.StringQuestionValueType = Right V11.StringQuestionValueType
  upgrade ctx V10.NumberQuestionValueType = Right V11.NumberQuestionValueType
  upgrade ctx V10.DateQuestionValueType = Right V11.DateQuestionValueType
  upgrade ctx V10.TextQuestionValueType = Right V11.TextQuestionValueType

instance Upgradeable V10.MetricMeasure V11.MetricMeasure

instance Upgradeable V10.AddKnowledgeModelEvent V11.AddKnowledgeModelEvent where
  upgrade ctx V10.AddKnowledgeModelEvent {..} =
    Right $
    V11.AddKnowledgeModelEvent
      _addKnowledgeModelEventUuid
      _addKnowledgeModelEventParentUuid
      _addKnowledgeModelEventEntityUuid
      (mapToList _addKnowledgeModelEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.EditKnowledgeModelEvent V11.EditKnowledgeModelEvent where
  upgrade ctx V10.EditKnowledgeModelEvent {..} =
    V11.EditKnowledgeModelEvent
      _editKnowledgeModelEventUuid
      _editKnowledgeModelEventParentUuid
      _editKnowledgeModelEventEntityUuid <$>
    upgrade ctx _editKnowledgeModelEventAnnotations <*>
    upgrade ctx _editKnowledgeModelEventChapterUuids <*>
    upgrade ctx _editKnowledgeModelEventTagUuids <*>
    upgrade ctx _editKnowledgeModelEventIntegrationUuids <*>
    upgrade ctx _editKnowledgeModelEventMetricUuids <*>
    upgrade ctx _editKnowledgeModelEventPhaseUuids <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.AddChapterEvent V11.AddChapterEvent where
  upgrade ctx V10.AddChapterEvent {..} =
    Right $
    V11.AddChapterEvent
      _addChapterEventUuid
      _addChapterEventParentUuid
      _addChapterEventEntityUuid
      _addChapterEventTitle
      _addChapterEventText
      (mapToList _addChapterEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.EditChapterEvent V11.EditChapterEvent where
  upgrade ctx V10.EditChapterEvent {..} =
    V11.EditChapterEvent _editChapterEventUuid _editChapterEventParentUuid _editChapterEventEntityUuid <$>
    upgrade ctx _editChapterEventTitle <*>
    upgrade ctx _editChapterEventText <*>
    upgrade ctx _editChapterEventAnnotations <*>
    upgrade ctx _editChapterEventQuestionUuids <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeleteChapterEvent V11.DeleteChapterEvent where
  upgrade ctx V10.DeleteChapterEvent {..} =
    Right $
    V11.DeleteChapterEvent
      _deleteChapterEventUuid
      _deleteChapterEventParentUuid
      _deleteChapterEventEntityUuid
      (ctx ^. createdAt)

instance Upgradeable V10.AddQuestionEvent V11.AddQuestionEvent where
  upgrade ctx (V10.AddOptionsQuestionEvent' event) = V11.AddOptionsQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddMultiChoiceQuestionEvent' event) = V11.AddMultiChoiceQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddListQuestionEvent' event) = V11.AddListQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddValueQuestionEvent' event) = V11.AddValueQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddIntegrationQuestionEvent' event) = V11.AddIntegrationQuestionEvent' <$> upgrade ctx event

instance Upgradeable V10.AddOptionsQuestionEvent V11.AddOptionsQuestionEvent where
  upgrade ctx V10.AddOptionsQuestionEvent {..} =
    Right $
    V11.AddOptionsQuestionEvent
      _addOptionsQuestionEventUuid
      _addOptionsQuestionEventParentUuid
      _addOptionsQuestionEventEntityUuid
      _addOptionsQuestionEventTitle
      _addOptionsQuestionEventText
      _addOptionsQuestionEventRequiredPhaseUuid
      (mapToList _addOptionsQuestionEventAnnotations)
      _addOptionsQuestionEventTagUuids
      (ctx ^. createdAt)

instance Upgradeable V10.AddMultiChoiceQuestionEvent V11.AddMultiChoiceQuestionEvent where
  upgrade ctx V10.AddMultiChoiceQuestionEvent {..} =
    Right $
    V11.AddMultiChoiceQuestionEvent
      _addMultiChoiceQuestionEventUuid
      _addMultiChoiceQuestionEventParentUuid
      _addMultiChoiceQuestionEventEntityUuid
      _addMultiChoiceQuestionEventTitle
      _addMultiChoiceQuestionEventText
      _addMultiChoiceQuestionEventRequiredPhaseUuid
      (mapToList _addMultiChoiceQuestionEventAnnotations)
      _addMultiChoiceQuestionEventTagUuids
      (ctx ^. createdAt)

instance Upgradeable V10.AddListQuestionEvent V11.AddListQuestionEvent where
  upgrade ctx V10.AddListQuestionEvent {..} =
    Right $
    V11.AddListQuestionEvent
      _addListQuestionEventUuid
      _addListQuestionEventParentUuid
      _addListQuestionEventEntityUuid
      _addListQuestionEventTitle
      _addListQuestionEventText
      _addListQuestionEventRequiredPhaseUuid
      (mapToList _addListQuestionEventAnnotations)
      _addListQuestionEventTagUuids
      (ctx ^. createdAt)

instance Upgradeable V10.AddValueQuestionEvent V11.AddValueQuestionEvent where
  upgrade ctx V10.AddValueQuestionEvent {..} =
    V11.AddValueQuestionEvent
      _addValueQuestionEventUuid
      _addValueQuestionEventParentUuid
      _addValueQuestionEventEntityUuid
      _addValueQuestionEventTitle
      _addValueQuestionEventText
      _addValueQuestionEventRequiredPhaseUuid
      (mapToList _addValueQuestionEventAnnotations)
      _addValueQuestionEventTagUuids <$>
    upgrade ctx _addValueQuestionEventValueType <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.AddIntegrationQuestionEvent V11.AddIntegrationQuestionEvent where
  upgrade ctx V10.AddIntegrationQuestionEvent {..} =
    Right $
    V11.AddIntegrationQuestionEvent
      _addIntegrationQuestionEventUuid
      _addIntegrationQuestionEventParentUuid
      _addIntegrationQuestionEventEntityUuid
      _addIntegrationQuestionEventTitle
      _addIntegrationQuestionEventText
      _addIntegrationQuestionEventRequiredPhaseUuid
      (mapToList _addIntegrationQuestionEventAnnotations)
      _addIntegrationQuestionEventTagUuids
      _addIntegrationQuestionEventIntegrationUuid
      _addIntegrationQuestionEventProps
      (ctx ^. createdAt)

instance Upgradeable V10.EditQuestionEvent V11.EditQuestionEvent where
  upgrade ctx (V10.EditOptionsQuestionEvent' event) = V11.EditOptionsQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditMultiChoiceQuestionEvent' event) = V11.EditMultiChoiceQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditListQuestionEvent' event) = V11.EditListQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditValueQuestionEvent' event) = V11.EditValueQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditIntegrationQuestionEvent' event) = V11.EditIntegrationQuestionEvent' <$> upgrade ctx event

instance Upgradeable V10.EditOptionsQuestionEvent V11.EditOptionsQuestionEvent where
  upgrade ctx V10.EditOptionsQuestionEvent {..} =
    V11.EditOptionsQuestionEvent
      _editOptionsQuestionEventUuid
      _editOptionsQuestionEventParentUuid
      _editOptionsQuestionEventEntityUuid <$>
    upgrade ctx _editOptionsQuestionEventTitle <*>
    upgrade ctx _editOptionsQuestionEventText <*>
    upgrade ctx _editOptionsQuestionEventRequiredPhaseUuid <*>
    upgrade ctx _editOptionsQuestionEventAnnotations <*>
    upgrade ctx _editOptionsQuestionEventTagUuids <*>
    upgrade ctx _editOptionsQuestionEventExpertUuids <*>
    upgrade ctx _editOptionsQuestionEventReferenceUuids <*>
    upgrade ctx _editOptionsQuestionEventAnswerUuids <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.EditMultiChoiceQuestionEvent V11.EditMultiChoiceQuestionEvent where
  upgrade ctx V10.EditMultiChoiceQuestionEvent {..} =
    V11.EditMultiChoiceQuestionEvent
      _editMultiChoiceQuestionEventUuid
      _editMultiChoiceQuestionEventParentUuid
      _editMultiChoiceQuestionEventEntityUuid <$>
    upgrade ctx _editMultiChoiceQuestionEventTitle <*>
    upgrade ctx _editMultiChoiceQuestionEventText <*>
    upgrade ctx _editMultiChoiceQuestionEventRequiredPhaseUuid <*>
    upgrade ctx _editMultiChoiceQuestionEventAnnotations <*>
    upgrade ctx _editMultiChoiceQuestionEventTagUuids <*>
    upgrade ctx _editMultiChoiceQuestionEventExpertUuids <*>
    upgrade ctx _editMultiChoiceQuestionEventReferenceUuids <*>
    upgrade ctx _editMultiChoiceQuestionEventChoiceUuids <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.EditListQuestionEvent V11.EditListQuestionEvent where
  upgrade ctx V10.EditListQuestionEvent {..} =
    V11.EditListQuestionEvent
      _editListQuestionEventUuid
      _editListQuestionEventParentUuid
      _editListQuestionEventEntityUuid <$>
    upgrade ctx _editListQuestionEventTitle <*>
    upgrade ctx _editListQuestionEventText <*>
    upgrade ctx _editListQuestionEventRequiredPhaseUuid <*>
    upgrade ctx _editListQuestionEventAnnotations <*>
    upgrade ctx _editListQuestionEventTagUuids <*>
    upgrade ctx _editListQuestionEventExpertUuids <*>
    upgrade ctx _editListQuestionEventReferenceUuids <*>
    upgrade ctx _editListQuestionEventItemTemplateQuestionUuids <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.EditValueQuestionEvent V11.EditValueQuestionEvent where
  upgrade ctx V10.EditValueQuestionEvent {..} =
    V11.EditValueQuestionEvent
      _editValueQuestionEventUuid
      _editValueQuestionEventParentUuid
      _editValueQuestionEventEntityUuid <$>
    upgrade ctx _editValueQuestionEventTitle <*>
    upgrade ctx _editValueQuestionEventText <*>
    upgrade ctx _editValueQuestionEventRequiredPhaseUuid <*>
    upgrade ctx _editValueQuestionEventAnnotations <*>
    upgrade ctx _editValueQuestionEventTagUuids <*>
    upgrade ctx _editValueQuestionEventExpertUuids <*>
    upgrade ctx _editValueQuestionEventReferenceUuids <*>
    upgrade ctx _editValueQuestionEventValueType <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.EditIntegrationQuestionEvent V11.EditIntegrationQuestionEvent where
  upgrade ctx V10.EditIntegrationQuestionEvent {..} =
    V11.EditIntegrationQuestionEvent
      _editIntegrationQuestionEventUuid
      _editIntegrationQuestionEventParentUuid
      _editIntegrationQuestionEventEntityUuid <$>
    upgrade ctx _editIntegrationQuestionEventTitle <*>
    upgrade ctx _editIntegrationQuestionEventText <*>
    upgrade ctx _editIntegrationQuestionEventRequiredPhaseUuid <*>
    upgrade ctx _editIntegrationQuestionEventAnnotations <*>
    upgrade ctx _editIntegrationQuestionEventTagUuids <*>
    upgrade ctx _editIntegrationQuestionEventExpertUuids <*>
    upgrade ctx _editIntegrationQuestionEventReferenceUuids <*>
    upgrade ctx _editIntegrationQuestionEventIntegrationUuid <*>
    upgrade ctx _editIntegrationQuestionEventProps <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeleteQuestionEvent V11.DeleteQuestionEvent where
  upgrade ctx V10.DeleteQuestionEvent {..} =
    Right $
    V11.DeleteQuestionEvent
      _deleteQuestionEventUuid
      _deleteQuestionEventParentUuid
      _deleteQuestionEventEntityUuid
      (ctx ^. createdAt)

instance Upgradeable V10.AddAnswerEvent V11.AddAnswerEvent where
  upgrade ctx V10.AddAnswerEvent {..} =
    V11.AddAnswerEvent
      _addAnswerEventUuid
      _addAnswerEventParentUuid
      _addAnswerEventEntityUuid
      _addAnswerEventLabel
      _addAnswerEventAdvice
      (mapToList _addAnswerEventAnnotations) <$>
    upgrade ctx _addAnswerEventMetricMeasures <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.EditAnswerEvent V11.EditAnswerEvent where
  upgrade ctx V10.EditAnswerEvent {..} =
    V11.EditAnswerEvent _editAnswerEventUuid _editAnswerEventParentUuid _editAnswerEventEntityUuid <$>
    upgrade ctx _editAnswerEventLabel <*>
    upgrade ctx _editAnswerEventAdvice <*>
    upgrade ctx _editAnswerEventAnnotations <*>
    upgrade ctx _editAnswerEventFollowUpUuids <*>
    upgrade ctx _editAnswerEventMetricMeasures <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeleteAnswerEvent V11.DeleteAnswerEvent where
  upgrade ctx V10.DeleteAnswerEvent {..} =
    Right $
    V11.DeleteAnswerEvent
      _deleteAnswerEventUuid
      _deleteAnswerEventParentUuid
      _deleteAnswerEventEntityUuid
      (ctx ^. createdAt)

instance Upgradeable V10.AddChoiceEvent V11.AddChoiceEvent where
  upgrade ctx V10.AddChoiceEvent {..} =
    Right $
    V11.AddChoiceEvent
      _addChoiceEventUuid
      _addChoiceEventParentUuid
      _addChoiceEventEntityUuid
      _addChoiceEventLabel
      (mapToList _addChoiceEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.EditChoiceEvent V11.EditChoiceEvent where
  upgrade ctx V10.EditChoiceEvent {..} =
    V11.EditChoiceEvent _editChoiceEventUuid _editChoiceEventParentUuid _editChoiceEventEntityUuid <$>
    upgrade ctx _editChoiceEventLabel <*>
    upgrade ctx _editChoiceEventAnnotations <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeleteChoiceEvent V11.DeleteChoiceEvent where
  upgrade ctx V10.DeleteChoiceEvent {..} =
    Right $
    V11.DeleteChoiceEvent
      _deleteChoiceEventUuid
      _deleteChoiceEventParentUuid
      _deleteChoiceEventEntityUuid
      (ctx ^. createdAt)

instance Upgradeable V10.AddExpertEvent V11.AddExpertEvent where
  upgrade ctx V10.AddExpertEvent {..} =
    Right $
    V11.AddExpertEvent
      _addExpertEventUuid
      _addExpertEventParentUuid
      _addExpertEventEntityUuid
      _addExpertEventName
      _addExpertEventEmail
      (mapToList _addExpertEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.EditExpertEvent V11.EditExpertEvent where
  upgrade ctx V10.EditExpertEvent {..} =
    V11.EditExpertEvent _editExpertEventUuid _editExpertEventParentUuid _editExpertEventEntityUuid <$>
    upgrade ctx _editExpertEventName <*>
    upgrade ctx _editExpertEventEmail <*>
    upgrade ctx _editExpertEventAnnotations <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeleteExpertEvent V11.DeleteExpertEvent where
  upgrade ctx V10.DeleteExpertEvent {..} =
    Right $
    V11.DeleteExpertEvent
      _deleteExpertEventUuid
      _deleteExpertEventParentUuid
      _deleteExpertEventEntityUuid
      (ctx ^. createdAt)

instance Upgradeable V10.AddReferenceEvent V11.AddReferenceEvent where
  upgrade ctx (V10.AddResourcePageReferenceEvent' event) = V11.AddResourcePageReferenceEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddURLReferenceEvent' event) = V11.AddURLReferenceEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddCrossReferenceEvent' event) = V11.AddCrossReferenceEvent' <$> upgrade ctx event

instance Upgradeable V10.AddResourcePageReferenceEvent V11.AddResourcePageReferenceEvent where
  upgrade ctx V10.AddResourcePageReferenceEvent {..} =
    Right $
    V11.AddResourcePageReferenceEvent
      _addResourcePageReferenceEventUuid
      _addResourcePageReferenceEventParentUuid
      _addResourcePageReferenceEventEntityUuid
      _addResourcePageReferenceEventShortUuid
      (mapToList _addResourcePageReferenceEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.AddURLReferenceEvent V11.AddURLReferenceEvent where
  upgrade ctx V10.AddURLReferenceEvent {..} =
    Right $
    V11.AddURLReferenceEvent
      _addURLReferenceEventUuid
      _addURLReferenceEventParentUuid
      _addURLReferenceEventEntityUuid
      _addURLReferenceEventUrl
      _addURLReferenceEventLabel
      (mapToList _addURLReferenceEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.AddCrossReferenceEvent V11.AddCrossReferenceEvent where
  upgrade ctx V10.AddCrossReferenceEvent {..} =
    Right $
    V11.AddCrossReferenceEvent
      _addCrossReferenceEventUuid
      _addCrossReferenceEventParentUuid
      _addCrossReferenceEventEntityUuid
      _addCrossReferenceEventTargetUuid
      _addCrossReferenceEventDescription
      (mapToList _addCrossReferenceEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.EditReferenceEvent V11.EditReferenceEvent where
  upgrade ctx (V10.EditResourcePageReferenceEvent' event) = V11.EditResourcePageReferenceEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditURLReferenceEvent' event) = V11.EditURLReferenceEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditCrossReferenceEvent' event) = V11.EditCrossReferenceEvent' <$> upgrade ctx event

instance Upgradeable V10.EditResourcePageReferenceEvent V11.EditResourcePageReferenceEvent where
  upgrade ctx V10.EditResourcePageReferenceEvent {..} =
    V11.EditResourcePageReferenceEvent
      _editResourcePageReferenceEventUuid
      _editResourcePageReferenceEventParentUuid
      _editResourcePageReferenceEventEntityUuid <$>
    upgrade ctx _editResourcePageReferenceEventShortUuid <*>
    upgrade ctx _editResourcePageReferenceEventAnnotations <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.EditURLReferenceEvent V11.EditURLReferenceEvent where
  upgrade ctx V10.EditURLReferenceEvent {..} =
    V11.EditURLReferenceEvent
      _editURLReferenceEventUuid
      _editURLReferenceEventParentUuid
      _editURLReferenceEventEntityUuid <$>
    upgrade ctx _editURLReferenceEventUrl <*>
    upgrade ctx _editURLReferenceEventLabel <*>
    upgrade ctx _editURLReferenceEventAnnotations <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.EditCrossReferenceEvent V11.EditCrossReferenceEvent where
  upgrade ctx V10.EditCrossReferenceEvent {..} =
    V11.EditCrossReferenceEvent
      _editCrossReferenceEventUuid
      _editCrossReferenceEventParentUuid
      _editCrossReferenceEventEntityUuid <$>
    upgrade ctx _editCrossReferenceEventTargetUuid <*>
    upgrade ctx _editCrossReferenceEventDescription <*>
    upgrade ctx _editCrossReferenceEventAnnotations <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeleteReferenceEvent V11.DeleteReferenceEvent where
  upgrade ctx V10.DeleteReferenceEvent {..} =
    Right $
    V11.DeleteReferenceEvent
      _deleteReferenceEventUuid
      _deleteReferenceEventParentUuid
      _deleteReferenceEventEntityUuid
      (ctx ^. createdAt)

instance Upgradeable V10.AddTagEvent V11.AddTagEvent where
  upgrade ctx V10.AddTagEvent {..} =
    Right $
    V11.AddTagEvent
      _addTagEventUuid
      _addTagEventParentUuid
      _addTagEventEntityUuid
      _addTagEventName
      _addTagEventDescription
      _addTagEventColor
      (mapToList _addTagEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.EditTagEvent V11.EditTagEvent where
  upgrade ctx V10.EditTagEvent {..} =
    V11.EditTagEvent _editTagEventUuid _editTagEventParentUuid _editTagEventEntityUuid <$> upgrade ctx _editTagEventName <*>
    upgrade ctx _editTagEventDescription <*>
    upgrade ctx _editTagEventColor <*>
    upgrade ctx _editTagEventAnnotations <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeleteTagEvent V11.DeleteTagEvent where
  upgrade ctx V10.DeleteTagEvent {..} =
    Right $
    V11.DeleteTagEvent _deleteTagEventUuid _deleteTagEventParentUuid _deleteTagEventEntityUuid (ctx ^. createdAt)

instance Upgradeable V10.AddIntegrationEvent V11.AddIntegrationEvent where
  upgrade ctx V10.AddIntegrationEvent {..} =
    Right $
    V11.AddIntegrationEvent
      _addIntegrationEventUuid
      _addIntegrationEventParentUuid
      _addIntegrationEventEntityUuid
      _addIntegrationEventIId
      _addIntegrationEventName
      _addIntegrationEventProps
      _addIntegrationEventLogo
      _addIntegrationEventRequestMethod
      _addIntegrationEventRequestUrl
      (mapToList _addIntegrationEventRequestHeaders)
      _addIntegrationEventRequestBody
      _addIntegrationEventResponseListField
      _addIntegrationEventResponseItemUrl
      _addIntegrationEventResponseItemId
      _addIntegrationEventResponseItemTemplate
      (mapToList _addIntegrationEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.EditIntegrationEvent V11.EditIntegrationEvent where
  upgrade ctx V10.EditIntegrationEvent {..} =
    V11.EditIntegrationEvent _editIntegrationEventUuid _editIntegrationEventParentUuid _editIntegrationEventEntityUuid <$>
    upgrade ctx _editIntegrationEventIId <*>
    upgrade ctx _editIntegrationEventName <*>
    upgrade ctx _editIntegrationEventProps <*>
    upgrade ctx _editIntegrationEventLogo <*>
    upgrade ctx _editIntegrationEventRequestMethod <*>
    upgrade ctx _editIntegrationEventRequestUrl <*>
    upgrade ctx _editIntegrationEventRequestHeaders <*>
    upgrade ctx _editIntegrationEventRequestBody <*>
    upgrade ctx _editIntegrationEventResponseListField <*>
    upgrade ctx _editIntegrationEventResponseItemUrl <*>
    upgrade ctx _editIntegrationEventResponseItemId <*>
    upgrade ctx _editIntegrationEventResponseItemTemplate <*>
    upgrade ctx _editIntegrationEventAnnotations <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeleteIntegrationEvent V11.DeleteIntegrationEvent where
  upgrade ctx V10.DeleteIntegrationEvent {..} =
    Right $
    V11.DeleteIntegrationEvent
      _deleteIntegrationEventUuid
      _deleteIntegrationEventParentUuid
      _deleteIntegrationEventEntityUuid
      (ctx ^. createdAt)

instance Upgradeable V10.AddMetricEvent V11.AddMetricEvent where
  upgrade ctx V10.AddMetricEvent {..} =
    Right $
    V11.AddMetricEvent
      _addMetricEventUuid
      _addMetricEventParentUuid
      _addMetricEventEntityUuid
      _addMetricEventTitle
      _addMetricEventAbbreviation
      _addMetricEventDescription
      (mapToList _addMetricEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.EditMetricEvent V11.EditMetricEvent where
  upgrade ctx V10.EditMetricEvent {..} =
    V11.EditMetricEvent _editMetricEventUuid _editMetricEventParentUuid _editMetricEventEntityUuid <$>
    upgrade ctx _editMetricEventTitle <*>
    upgrade ctx _editMetricEventAbbreviation <*>
    upgrade ctx _editMetricEventDescription <*>
    upgrade ctx _editMetricEventAnnotations <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeleteMetricEvent V11.DeleteMetricEvent where
  upgrade ctx V10.DeleteMetricEvent {..} =
    Right $
    V11.DeleteMetricEvent
      _deleteMetricEventUuid
      _deleteMetricEventParentUuid
      _deleteMetricEventEntityUuid
      (ctx ^. createdAt)

instance Upgradeable V10.AddPhaseEvent V11.AddPhaseEvent where
  upgrade ctx V10.AddPhaseEvent {..} =
    Right $
    V11.AddPhaseEvent
      _addPhaseEventUuid
      _addPhaseEventParentUuid
      _addPhaseEventEntityUuid
      _addPhaseEventTitle
      _addPhaseEventDescription
      (mapToList _addPhaseEventAnnotations)
      (ctx ^. createdAt)

instance Upgradeable V10.EditPhaseEvent V11.EditPhaseEvent where
  upgrade ctx V10.EditPhaseEvent {..} =
    V11.EditPhaseEvent _editPhaseEventUuid _editPhaseEventParentUuid _editPhaseEventEntityUuid <$>
    upgrade ctx _editPhaseEventTitle <*>
    upgrade ctx _editPhaseEventDescription <*>
    upgrade ctx _editPhaseEventAnnotations <*>
    Right (ctx ^. createdAt)

instance Upgradeable V10.DeletePhaseEvent V11.DeletePhaseEvent where
  upgrade ctx V10.DeletePhaseEvent {..} =
    Right $
    V11.DeletePhaseEvent
      _deletePhaseEventUuid
      _deletePhaseEventParentUuid
      _deletePhaseEventEntityUuid
      (ctx ^. createdAt)

instance Upgradeable V10.MoveQuestionEvent V11.MoveQuestionEvent where
  upgrade ctx V10.MoveQuestionEvent {..} =
    Right $
    V11.MoveQuestionEvent
      _moveQuestionEventUuid
      _moveQuestionEventParentUuid
      _moveQuestionEventEntityUuid
      _moveQuestionEventTargetUuid
      (ctx ^. createdAt)

instance Upgradeable V10.MoveAnswerEvent V11.MoveAnswerEvent where
  upgrade ctx V10.MoveAnswerEvent {..} =
    Right $
    V11.MoveAnswerEvent
      _moveAnswerEventUuid
      _moveAnswerEventParentUuid
      _moveAnswerEventEntityUuid
      _moveAnswerEventTargetUuid
      (ctx ^. createdAt)

instance Upgradeable V10.MoveChoiceEvent V11.MoveChoiceEvent where
  upgrade ctx V10.MoveChoiceEvent {..} =
    Right $
    V11.MoveChoiceEvent
      _moveChoiceEventUuid
      _moveChoiceEventParentUuid
      _moveChoiceEventEntityUuid
      _moveChoiceEventTargetUuid
      (ctx ^. createdAt)

instance Upgradeable V10.MoveExpertEvent V11.MoveExpertEvent where
  upgrade ctx V10.MoveExpertEvent {..} =
    Right $
    V11.MoveExpertEvent
      _moveExpertEventUuid
      _moveExpertEventParentUuid
      _moveExpertEventEntityUuid
      _moveExpertEventTargetUuid
      (ctx ^. createdAt)

instance Upgradeable V10.MoveReferenceEvent V11.MoveReferenceEvent where
  upgrade ctx V10.MoveReferenceEvent {..} =
    Right $
    V11.MoveReferenceEvent
      _moveReferenceEventUuid
      _moveReferenceEventParentUuid
      _moveReferenceEventEntityUuid
      _moveReferenceEventTargetUuid
      (ctx ^. createdAt)

instance Upgradeable V10.Event V11.Event where
  upgrade ctx (V10.AddKnowledgeModelEvent' event) = V11.AddKnowledgeModelEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditKnowledgeModelEvent' event) = V11.EditKnowledgeModelEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddChapterEvent' event) = V11.AddChapterEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditChapterEvent' event) = V11.EditChapterEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeleteChapterEvent' event) = V11.DeleteChapterEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddQuestionEvent' event) = V11.AddQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditQuestionEvent' event) = V11.EditQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeleteQuestionEvent' event) = V11.DeleteQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddAnswerEvent' event) = V11.AddAnswerEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditAnswerEvent' event) = V11.EditAnswerEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeleteAnswerEvent' event) = V11.DeleteAnswerEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddChoiceEvent' event) = V11.AddChoiceEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditChoiceEvent' event) = V11.EditChoiceEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeleteChoiceEvent' event) = V11.DeleteChoiceEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddExpertEvent' event) = V11.AddExpertEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditExpertEvent' event) = V11.EditExpertEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeleteExpertEvent' event) = V11.DeleteExpertEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddReferenceEvent' event) = V11.AddReferenceEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditReferenceEvent' event) = V11.EditReferenceEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeleteReferenceEvent' event) = V11.DeleteReferenceEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddTagEvent' event) = V11.AddTagEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditTagEvent' event) = V11.EditTagEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeleteTagEvent' event) = V11.DeleteTagEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddIntegrationEvent' event) = V11.AddIntegrationEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditIntegrationEvent' event) = V11.EditIntegrationEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeleteIntegrationEvent' event) = V11.DeleteIntegrationEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddMetricEvent' event) = V11.AddMetricEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditMetricEvent' event) = V11.EditMetricEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeleteMetricEvent' event) = V11.DeleteMetricEvent' <$> upgrade ctx event
  upgrade ctx (V10.AddPhaseEvent' event) = V11.AddPhaseEvent' <$> upgrade ctx event
  upgrade ctx (V10.EditPhaseEvent' event) = V11.EditPhaseEvent' <$> upgrade ctx event
  upgrade ctx (V10.DeletePhaseEvent' event) = V11.DeletePhaseEvent' <$> upgrade ctx event
  upgrade ctx (V10.MoveQuestionEvent' event) = V11.MoveQuestionEvent' <$> upgrade ctx event
  upgrade ctx (V10.MoveAnswerEvent' event) = V11.MoveAnswerEvent' <$> upgrade ctx event
  upgrade ctx (V10.MoveChoiceEvent' event) = V11.MoveChoiceEvent' <$> upgrade ctx event
  upgrade ctx (V10.MoveExpertEvent' event) = V11.MoveExpertEvent' <$> upgrade ctx event
  upgrade ctx (V10.MoveReferenceEvent' event) = V11.MoveReferenceEvent' <$> upgrade ctx event

migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue ctx input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade ctx (oldEvent :: V10.Event)
  return [toJSON (newEvent :: V11.Event)]
