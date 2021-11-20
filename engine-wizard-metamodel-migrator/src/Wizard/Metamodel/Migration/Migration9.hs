module Wizard.Metamodel.Migration.Migration9 where

import Data.Aeson (FromJSON, Result(..), ToJSON(toJSON), Value, fromJSON)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import qualified Wizard.Metamodel.Event.Version10 as V10
import qualified Wizard.Metamodel.Event.Version10.Common as V10
import qualified Wizard.Metamodel.Event.Version10.Integration as V10
import qualified Wizard.Metamodel.Event.Version9 as V9
import qualified Wizard.Metamodel.Event.Version9.Common as V9
import qualified Wizard.Metamodel.Event.Version9.Integration as V9

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

instance (Upgradeable f t) => Upgradeable (V9.EventField f) (V10.EventField t) where
  upgrade V9.NothingChanged = Right V10.NothingChanged
  upgrade (V9.ChangedValue x) = V10.ChangedValue <$> upgrade x

navToTemplate :: String -> String
navToTemplate str = "{{response." ++ str ++ "}}"

navToTemplateEdit :: V9.EventField String -> Either String (V10.EventField String)
navToTemplateEdit V9.NothingChanged = Right V10.NothingChanged
navToTemplateEdit (V9.ChangedValue x) = Right . V10.ChangedValue . navToTemplate $ x

instance Upgradeable V9.AddIntegrationEvent V10.AddIntegrationEvent where
  upgrade V9.AddIntegrationEvent {..} =
    Right $
    V10.AddIntegrationEvent
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
      _addIntegrationEventItemUrl
      (navToTemplate _addIntegrationEventResponseIdField)
      (navToTemplate _addIntegrationEventResponseNameField)
      Nothing
      _addIntegrationEventAnnotations

instance Upgradeable V9.EditIntegrationEvent V10.EditIntegrationEvent where
  upgrade V9.EditIntegrationEvent {..} =
    V10.EditIntegrationEvent _editIntegrationEventUuid _editIntegrationEventParentUuid _editIntegrationEventEntityUuid <$>
    upgrade _editIntegrationEventIId <*>
    upgrade _editIntegrationEventName <*>
    upgrade _editIntegrationEventProps <*>
    upgrade _editIntegrationEventLogo <*>
    upgrade _editIntegrationEventRequestMethod <*>
    upgrade _editIntegrationEventRequestUrl <*>
    upgrade _editIntegrationEventRequestHeaders <*>
    upgrade _editIntegrationEventRequestBody <*>
    upgrade _editIntegrationEventResponseListField <*>
    upgrade _editIntegrationEventItemUrl <*>
    navToTemplateEdit _editIntegrationEventResponseIdField <*>
    navToTemplateEdit _editIntegrationEventResponseNameField <*>
    Right V10.NothingChanged <*>
    upgrade _editIntegrationEventAnnotations

instance Upgradeable V9.Event V10.Event where
  upgrade (V9.AddKnowledgeModelEvent' event) = V10.AddKnowledgeModelEvent' <$> defaultUpgrade event
  upgrade (V9.EditKnowledgeModelEvent' event) = V10.EditKnowledgeModelEvent' <$> defaultUpgrade event
  upgrade (V9.AddChapterEvent' event) = V10.AddChapterEvent' <$> defaultUpgrade event
  upgrade (V9.EditChapterEvent' event) = V10.EditChapterEvent' <$> defaultUpgrade event
  upgrade (V9.DeleteChapterEvent' event) = V10.DeleteChapterEvent' <$> defaultUpgrade event
  upgrade (V9.AddQuestionEvent' event) = V10.AddQuestionEvent' <$> defaultUpgrade event
  upgrade (V9.EditQuestionEvent' event) = V10.EditQuestionEvent' <$> defaultUpgrade event
  upgrade (V9.DeleteQuestionEvent' event) = V10.DeleteQuestionEvent' <$> defaultUpgrade event
  upgrade (V9.AddAnswerEvent' event) = V10.AddAnswerEvent' <$> defaultUpgrade event
  upgrade (V9.EditAnswerEvent' event) = V10.EditAnswerEvent' <$> defaultUpgrade event
  upgrade (V9.DeleteAnswerEvent' event) = V10.DeleteAnswerEvent' <$> defaultUpgrade event
  upgrade (V9.AddChoiceEvent' event) = V10.AddChoiceEvent' <$> defaultUpgrade event
  upgrade (V9.EditChoiceEvent' event) = V10.EditChoiceEvent' <$> defaultUpgrade event
  upgrade (V9.DeleteChoiceEvent' event) = V10.DeleteChoiceEvent' <$> defaultUpgrade event
  upgrade (V9.AddExpertEvent' event) = V10.AddExpertEvent' <$> defaultUpgrade event
  upgrade (V9.EditExpertEvent' event) = V10.EditExpertEvent' <$> defaultUpgrade event
  upgrade (V9.DeleteExpertEvent' event) = V10.DeleteExpertEvent' <$> defaultUpgrade event
  upgrade (V9.AddReferenceEvent' event) = V10.AddReferenceEvent' <$> defaultUpgrade event
  upgrade (V9.EditReferenceEvent' event) = V10.EditReferenceEvent' <$> defaultUpgrade event
  upgrade (V9.DeleteReferenceEvent' event) = V10.DeleteReferenceEvent' <$> defaultUpgrade event
  upgrade (V9.AddTagEvent' event) = V10.AddTagEvent' <$> defaultUpgrade event
  upgrade (V9.EditTagEvent' event) = V10.EditTagEvent' <$> defaultUpgrade event
  upgrade (V9.DeleteTagEvent' event) = V10.DeleteTagEvent' <$> defaultUpgrade event
  upgrade (V9.AddIntegrationEvent' event) = V10.AddIntegrationEvent' <$> upgrade event
  upgrade (V9.EditIntegrationEvent' event) = V10.EditIntegrationEvent' <$> upgrade event
  upgrade (V9.DeleteIntegrationEvent' event) = V10.DeleteIntegrationEvent' <$> defaultUpgrade event
  upgrade (V9.AddMetricEvent' event) = V10.AddMetricEvent' <$> defaultUpgrade event
  upgrade (V9.EditMetricEvent' event) = V10.EditMetricEvent' <$> defaultUpgrade event
  upgrade (V9.DeleteMetricEvent' event) = V10.DeleteMetricEvent' <$> defaultUpgrade event
  upgrade (V9.AddPhaseEvent' event) = V10.AddPhaseEvent' <$> defaultUpgrade event
  upgrade (V9.EditPhaseEvent' event) = V10.EditPhaseEvent' <$> defaultUpgrade event
  upgrade (V9.DeletePhaseEvent' event) = V10.DeletePhaseEvent' <$> defaultUpgrade event
  upgrade (V9.MoveQuestionEvent' event) = V10.MoveQuestionEvent' <$> defaultUpgrade event
  upgrade (V9.MoveAnswerEvent' event) = V10.MoveAnswerEvent' <$> defaultUpgrade event
  upgrade (V9.MoveChoiceEvent' event) = V10.MoveChoiceEvent' <$> defaultUpgrade event
  upgrade (V9.MoveExpertEvent' event) = V10.MoveExpertEvent' <$> defaultUpgrade event
  upgrade (V9.MoveReferenceEvent' event) = V10.MoveReferenceEvent' <$> defaultUpgrade event

migrateEventValue :: Value -> Either String [Value]
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V9.Event)
  return [toJSON (newEvent :: V10.Event)]
