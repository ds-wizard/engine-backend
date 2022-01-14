module Wizard.Metamodel.Migration.Migration0007 where

import Data.Aeson (FromJSON, Result(..), ToJSON(toJSON), Value, fromJSON)
import Data.Either (partitionEithers)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.UUID as U

import qualified Wizard.Metamodel.Event.Version0007 as V7
import qualified Wizard.Metamodel.Event.Version0007.Common as V7
import qualified Wizard.Metamodel.Event.Version0007.KnowledgeModel as V7
import qualified Wizard.Metamodel.Event.Version0007.Question as V7
import qualified Wizard.Metamodel.Event.Version0008 as V8
import qualified Wizard.Metamodel.Event.Version0008.Common as V8
import qualified Wizard.Metamodel.Event.Version0008.KnowledgeModel as V8
import qualified Wizard.Metamodel.Event.Version0008.Metric as V8
import qualified Wizard.Metamodel.Event.Version0008.Phase as V8
import qualified Wizard.Metamodel.Event.Version0008.Question as V8
import Wizard.Metamodel.Migration.MigrationContext

u' :: String -> UUID
u' = fromJust . U.fromString

-- Phases
uuidPhase1 :: U.UUID
uuidPhase1 = u' "b101f2d0-2476-452d-aa8d-95a41a02b52c"

uuidPhase2 :: U.UUID
uuidPhase2 = u' "1796fa3c-9f53-475f-89ff-c66a0453c42e"

uuidPhase3 :: U.UUID
uuidPhase3 = u' "adc9133d-afcd-4616-9aea-db5f475898a2"

uuidPhase4 :: U.UUID
uuidPhase4 = u' "1ace0fc6-a949-495f-a32e-e948f3f6bed1"

addPhase1 :: U.UUID -> V8.AddPhaseEvent
addPhase1 parentUuid =
  V8.AddPhaseEvent
    { _addPhaseEventUuid = u' "eff6bbfc-8983-4799-8b89-f7a4ea37b611"
    , _addPhaseEventParentUuid = parentUuid
    , _addPhaseEventEntityUuid = uuidPhase1
    , _addPhaseEventTitle = "Before Submitting the Proposal"
    , _addPhaseEventDescription = Nothing
    }

addPhase2 :: U.UUID -> V8.AddPhaseEvent
addPhase2 parentUuid =
  V8.AddPhaseEvent
    { _addPhaseEventUuid = u' "e9bfcd64-411e-424a-98c9-5b4d531f0890"
    , _addPhaseEventParentUuid = parentUuid
    , _addPhaseEventEntityUuid = uuidPhase2
    , _addPhaseEventTitle = "Before Submitting the DMP"
    , _addPhaseEventDescription = Nothing
    }

addPhase3 :: U.UUID -> V8.AddPhaseEvent
addPhase3 parentUuid =
  V8.AddPhaseEvent
    { _addPhaseEventUuid = u' "bd22e504-295a-4ade-8fca-7f86aacddd76"
    , _addPhaseEventParentUuid = parentUuid
    , _addPhaseEventEntityUuid = uuidPhase3
    , _addPhaseEventTitle = "Before Finishing the Project"
    , _addPhaseEventDescription = Nothing
    }

addPhase4 :: U.UUID -> V8.AddPhaseEvent
addPhase4 parentUuid =
  V8.AddPhaseEvent
    { _addPhaseEventUuid = u' "fc62249c-700a-45d2-97b8-e5817468a5d5"
    , _addPhaseEventParentUuid = parentUuid
    , _addPhaseEventEntityUuid = uuidPhase4
    , _addPhaseEventTitle = "After Finishing the Project"
    , _addPhaseEventDescription = Nothing
    }

addPhases :: U.UUID -> [V8.AddPhaseEvent]
addPhases x = map ($ x) [addPhase1, addPhase2, addPhase3, addPhase4]

phaseUpgrade :: Maybe Int -> Maybe U.UUID
phaseUpgrade (Just 1) = Just uuidPhase1
phaseUpgrade (Just 2) = Just uuidPhase2
phaseUpgrade (Just 3) = Just uuidPhase3
phaseUpgrade (Just 4) = Just uuidPhase4
phaseUpgrade _ = Nothing

phaseChangeUpgrade :: V7.EventField (Maybe Int) -> Either String (V8.EventField (Maybe U.UUID))
phaseChangeUpgrade V7.NothingChanged = Right V8.NothingChanged
phaseChangeUpgrade (V7.ChangedValue x) = Right . V8.ChangedValue . phaseUpgrade $ x

-- Metrics
addMetricF :: U.UUID -> V8.AddMetricEvent
addMetricF parentUuid =
  V8.AddMetricEvent
    { _addMetricEventUuid = u' "b630056c-3acf-4f26-b317-50f1805402c0"
    , _addMetricEventParentUuid = parentUuid
    , _addMetricEventEntityUuid = u' "8db30660-d4e5-4c0a-bf3e-553f3f0f997a"
    , _addMetricEventTitle = "Findability"
    , _addMetricEventAbbreviation = Just "F"
    , _addMetricEventDescription =
        Just
          "The Findability metric describes how easily data can be located. The score associated with an answer will be higher if it makes it easier for humans or for computers to locate your data set, e.g. if it ends up in an index or has a unique resolvable identifier."
    }

addMetricA :: U.UUID -> V8.AddMetricEvent
addMetricA parentUuid =
  V8.AddMetricEvent
    { _addMetricEventUuid = u' "c19feec6-2ab2-4757-893a-c11c47f352b8"
    , _addMetricEventParentUuid = parentUuid
    , _addMetricEventEntityUuid = u' "0feac7e6-add4-4723-abae-be5ce7864c63"
    , _addMetricEventTitle = "Accessibility"
    , _addMetricEventAbbreviation = Just "A"
    , _addMetricEventDescription =
        Just
          "The Accessibility metric describes how well the access to the database is described and how easy it is to implement. The score associated with an answer will be higher if it makes it easier for humans and computers to get to the data. This is determined by e.g. the protocol for accessing the data or for authenticating users, and also by the guaranteed longevity of the repository. Note that this is different from the Openness metric!"
    }

addMetricI :: U.UUID -> V8.AddMetricEvent
addMetricI parentUuid =
  V8.AddMetricEvent
    { _addMetricEventUuid = u' "412854a9-ee08-4630-8e9a-71978c8290b3"
    , _addMetricEventParentUuid = parentUuid
    , _addMetricEventEntityUuid = u' "a42bded3-a085-45f8-b384-32b4a77c8385"
    , _addMetricEventTitle = "Interoperability"
    , _addMetricEventAbbreviation = Just "I"
    , _addMetricEventDescription =
        Just
          "The Interoperability metric describes how well the data interoperates with other data. The score associated with an answer will be higher if it makes it easier for humans and computers to couple the data with other data and 'understand' relationships. This is influenced by the use of standard ontologies for different fields and proper descriptions of the relations. It is also influenced by proper standard metadata that is agreed by the community."
    }

addMetricR :: U.UUID -> V8.AddMetricEvent
addMetricR parentUuid =
  V8.AddMetricEvent
    { _addMetricEventUuid = u' "b0bf9ac7-cd4c-4e77-9d56-2363e558bc8e"
    , _addMetricEventParentUuid = parentUuid
    , _addMetricEventEntityUuid = u' "0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7"
    , _addMetricEventTitle = "Reusability"
    , _addMetricEventAbbreviation = Just "R"
    , _addMetricEventDescription =
        Just
          "The Reusability metric describes how well the data is suitable for reuse in other context. The score associated with an answer will be higher if it makes it easier for humans and computers to reuse the data. This is influenced largely by proper description of how the data was obtained, and also by the conditions that are put on the reuse (license and, for personally identifying information, consent)."
    }

addMetricG :: U.UUID -> V8.AddMetricEvent
addMetricG parentUuid =
  V8.AddMetricEvent
    { _addMetricEventUuid = u' "c99e578d-f4cc-4a36-94cc-1451901f11fe"
    , _addMetricEventParentUuid = parentUuid
    , _addMetricEventEntityUuid = u' "8845fe2b-79df-4138-baea-3a035bf5e249"
    , _addMetricEventTitle = "Good DMP Practice"
    , _addMetricEventAbbreviation = Just "G"
    , _addMetricEventDescription =
        Just
          "The Good DMP Practice metric describes how appreciated a process is among Data Stewards. A score associated with an answer will be high if a practice would be considered preferable over alternatives, generally a good idea."
    }

addMetricO :: U.UUID -> V8.AddMetricEvent
addMetricO parentUuid =
  V8.AddMetricEvent
    { _addMetricEventUuid = u' "04d738dd-3b71-4433-afd6-65b105fa71cd"
    , _addMetricEventParentUuid = parentUuid
    , _addMetricEventEntityUuid = u' "cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0"
    , _addMetricEventTitle = "Openness"
    , _addMetricEventAbbreviation = Just "O"
    , _addMetricEventDescription =
        Just
          "The Openness metric describes how Open the data are available. Note that this is different from the Accessibility metric. A score associated with an answer will be high if the data will be as open as possible, and low if voluntary restrictions apply to access and re-use."
    }

addMetrics :: U.UUID -> [V8.AddMetricEvent]
addMetrics x = map ($ x) [addMetricF, addMetricA, addMetricI, addMetricR, addMetricG, addMetricO]

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
  additional :: f -> t -> Either String [Value]
  additional _ _ = Right []

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

instance Upgradeable (M.Map String String) (M.Map String String) where
  upgrade = Right

instance Upgradeable String (Maybe String) where
  upgrade = Right . Just

instance (Upgradeable f t) => Upgradeable (V7.EventField f) (V8.EventField t) where
  upgrade V7.NothingChanged = Right V8.NothingChanged
  upgrade (V7.ChangedValue x) = V8.ChangedValue <$> upgrade x

instance Upgradeable V7.QuestionValueType V8.QuestionValueType where
  upgrade V7.StringQuestionValueType = Right V8.StringQuestionValueType
  upgrade V7.NumberQuestionValueType = Right V8.NumberQuestionValueType
  upgrade V7.DateQuestionValueType = Right V8.DateQuestionValueType
  upgrade V7.TextQuestionValueType = Right V8.TextQuestionValueType

instance Upgradeable V7.AddKnowledgeModelEvent V8.AddKnowledgeModelEvent where
  additional V7.AddKnowledgeModelEvent {..} _ = return $ phases ++ metrics
    where
      phases = map toJSON $ addPhases _addKnowledgeModelEventEntityUuid
      metrics = map toJSON $ addMetrics _addKnowledgeModelEventEntityUuid

instance Upgradeable V7.EditKnowledgeModelEvent V8.EditKnowledgeModelEvent where
  upgrade V7.EditKnowledgeModelEvent {..} =
    V8.EditKnowledgeModelEvent
      _editKnowledgeModelEventUuid
      _editKnowledgeModelEventParentUuid
      _editKnowledgeModelEventEntityUuid <$>
    upgrade _editKnowledgeModelEventChapterUuids <*>
    upgrade _editKnowledgeModelEventTagUuids <*>
    upgrade _editKnowledgeModelEventIntegrationUuids <*>
    Right V8.NothingChanged <*>
    Right V8.NothingChanged

instance Upgradeable V7.AddQuestionEvent V8.AddQuestionEvent where
  upgrade (V7.AddOptionsQuestionEvent' V7.AddOptionsQuestionEvent {..}) =
    Right $
    V8.AddOptionsQuestionEvent' $
    V8.AddOptionsQuestionEvent
      _addOptionsQuestionEventUuid
      _addOptionsQuestionEventParentUuid
      _addOptionsQuestionEventEntityUuid
      _addOptionsQuestionEventTitle
      _addOptionsQuestionEventText
      (phaseUpgrade _addOptionsQuestionEventRequiredLevel)
      _addOptionsQuestionEventTagUuids
  upgrade (V7.AddMultiChoiceQuestionEvent' V7.AddMultiChoiceQuestionEvent {..}) =
    Right $
    V8.AddMultiChoiceQuestionEvent' $
    V8.AddMultiChoiceQuestionEvent
      _addMultiChoiceQuestionEventUuid
      _addMultiChoiceQuestionEventParentUuid
      _addMultiChoiceQuestionEventEntityUuid
      _addMultiChoiceQuestionEventTitle
      _addMultiChoiceQuestionEventText
      (phaseUpgrade _addMultiChoiceQuestionEventRequiredLevel)
      _addMultiChoiceQuestionEventTagUuids
  upgrade (V7.AddListQuestionEvent' V7.AddListQuestionEvent {..}) =
    Right $
    V8.AddListQuestionEvent' $
    V8.AddListQuestionEvent
      _addListQuestionEventUuid
      _addListQuestionEventParentUuid
      _addListQuestionEventEntityUuid
      _addListQuestionEventTitle
      _addListQuestionEventText
      (phaseUpgrade _addListQuestionEventRequiredLevel)
      _addListQuestionEventTagUuids
  upgrade (V7.AddValueQuestionEvent' V7.AddValueQuestionEvent {..}) =
    V8.AddValueQuestionEvent' .
    V8.AddValueQuestionEvent
      _addValueQuestionEventUuid
      _addValueQuestionEventParentUuid
      _addValueQuestionEventEntityUuid
      _addValueQuestionEventTitle
      _addValueQuestionEventText
      (phaseUpgrade _addValueQuestionEventRequiredLevel)
      _addValueQuestionEventTagUuids <$>
    upgrade _addValueQuestionEventValueType
  upgrade (V7.AddIntegrationQuestionEvent' V7.AddIntegrationQuestionEvent {..}) =
    Right $
    V8.AddIntegrationQuestionEvent' $
    V8.AddIntegrationQuestionEvent
      _addIntegrationQuestionEventUuid
      _addIntegrationQuestionEventParentUuid
      _addIntegrationQuestionEventEntityUuid
      _addIntegrationQuestionEventTitle
      _addIntegrationQuestionEventText
      (phaseUpgrade _addIntegrationQuestionEventRequiredLevel)
      _addIntegrationQuestionEventTagUuids
      _addIntegrationQuestionEventIntegrationUuid
      _addIntegrationQuestionEventProps

instance Upgradeable V7.EditQuestionEvent V8.EditQuestionEvent where
  upgrade (V7.EditOptionsQuestionEvent' V7.EditOptionsQuestionEvent {..}) = V8.EditOptionsQuestionEvent' <$> e
    where
      e =
        V8.EditOptionsQuestionEvent
          _editOptionsQuestionEventUuid
          _editOptionsQuestionEventParentUuid
          _editOptionsQuestionEventEntityUuid <$>
        upgrade _editOptionsQuestionEventTitle <*>
        upgrade _editOptionsQuestionEventText <*>
        phaseChangeUpgrade _editOptionsQuestionEventRequiredLevel <*>
        upgrade _editOptionsQuestionEventTagUuids <*>
        upgrade _editOptionsQuestionEventExpertUuids <*>
        upgrade _editOptionsQuestionEventReferenceUuids <*>
        upgrade _editOptionsQuestionEventAnswerUuids
  upgrade (V7.EditMultiChoiceQuestionEvent' V7.EditMultiChoiceQuestionEvent {..}) =
    V8.EditMultiChoiceQuestionEvent' <$> e
    where
      e =
        V8.EditMultiChoiceQuestionEvent
          _editMultiChoiceQuestionEventUuid
          _editMultiChoiceQuestionEventParentUuid
          _editMultiChoiceQuestionEventEntityUuid <$>
        upgrade _editMultiChoiceQuestionEventTitle <*>
        upgrade _editMultiChoiceQuestionEventText <*>
        phaseChangeUpgrade _editMultiChoiceQuestionEventRequiredLevel <*>
        upgrade _editMultiChoiceQuestionEventTagUuids <*>
        upgrade _editMultiChoiceQuestionEventExpertUuids <*>
        upgrade _editMultiChoiceQuestionEventReferenceUuids <*>
        upgrade _editMultiChoiceQuestionEventChoiceUuids
  upgrade (V7.EditListQuestionEvent' V7.EditListQuestionEvent {..}) = V8.EditListQuestionEvent' <$> e
    where
      e =
        V8.EditListQuestionEvent
          _editListQuestionEventUuid
          _editListQuestionEventParentUuid
          _editListQuestionEventEntityUuid <$>
        upgrade _editListQuestionEventTitle <*>
        upgrade _editListQuestionEventText <*>
        phaseChangeUpgrade _editListQuestionEventRequiredLevel <*>
        upgrade _editListQuestionEventTagUuids <*>
        upgrade _editListQuestionEventExpertUuids <*>
        upgrade _editListQuestionEventReferenceUuids <*>
        upgrade _editListQuestionEventItemTemplateQuestionUuids
  upgrade (V7.EditValueQuestionEvent' V7.EditValueQuestionEvent {..}) = V8.EditValueQuestionEvent' <$> e
    where
      e =
        V8.EditValueQuestionEvent
          _editValueQuestionEventUuid
          _editValueQuestionEventParentUuid
          _editValueQuestionEventEntityUuid <$>
        upgrade _editValueQuestionEventTitle <*>
        upgrade _editValueQuestionEventText <*>
        phaseChangeUpgrade _editValueQuestionEventRequiredLevel <*>
        upgrade _editValueQuestionEventTagUuids <*>
        upgrade _editValueQuestionEventExpertUuids <*>
        upgrade _editValueQuestionEventReferenceUuids <*>
        upgrade _editValueQuestionEventValueType
  upgrade (V7.EditIntegrationQuestionEvent' V7.EditIntegrationQuestionEvent {..}) =
    V8.EditIntegrationQuestionEvent' <$> e
    where
      e =
        V8.EditIntegrationQuestionEvent
          _editIntegrationQuestionEventUuid
          _editIntegrationQuestionEventParentUuid
          _editIntegrationQuestionEventEntityUuid <$>
        upgrade _editIntegrationQuestionEventTitle <*>
        upgrade _editIntegrationQuestionEventText <*>
        phaseChangeUpgrade _editIntegrationQuestionEventRequiredLevel <*>
        upgrade _editIntegrationQuestionEventTagUuids <*>
        upgrade _editIntegrationQuestionEventExpertUuids <*>
        upgrade _editIntegrationQuestionEventReferenceUuids <*>
        upgrade _editIntegrationQuestionEventIntegrationUuid <*>
        upgrade _editIntegrationQuestionEventProps

instance Upgradeable V7.Event V8.Event where
  upgrade (V7.AddKnowledgeModelEvent' event) = V8.AddKnowledgeModelEvent' <$> defaultUpgrade event
  upgrade (V7.EditKnowledgeModelEvent' event) = V8.EditKnowledgeModelEvent' <$> upgrade event
  upgrade (V7.AddChapterEvent' event) = V8.AddChapterEvent' <$> defaultUpgrade event
  upgrade (V7.EditChapterEvent' event) = V8.EditChapterEvent' <$> defaultUpgrade event
  upgrade (V7.DeleteChapterEvent' event) = V8.DeleteChapterEvent' <$> defaultUpgrade event
  upgrade (V7.AddQuestionEvent' event) = V8.AddQuestionEvent' <$> upgrade event
  upgrade (V7.EditQuestionEvent' event) = V8.EditQuestionEvent' <$> upgrade event
  upgrade (V7.DeleteQuestionEvent' event) = V8.DeleteQuestionEvent' <$> defaultUpgrade event
  upgrade (V7.AddAnswerEvent' event) = V8.AddAnswerEvent' <$> defaultUpgrade event
  upgrade (V7.EditAnswerEvent' event) = V8.EditAnswerEvent' <$> defaultUpgrade event
  upgrade (V7.DeleteAnswerEvent' event) = V8.DeleteAnswerEvent' <$> defaultUpgrade event
  upgrade (V7.AddChoiceEvent' event) = V8.AddChoiceEvent' <$> defaultUpgrade event
  upgrade (V7.EditChoiceEvent' event) = V8.EditChoiceEvent' <$> defaultUpgrade event
  upgrade (V7.DeleteChoiceEvent' event) = V8.DeleteChoiceEvent' <$> defaultUpgrade event
  upgrade (V7.AddExpertEvent' event) = V8.AddExpertEvent' <$> defaultUpgrade event
  upgrade (V7.EditExpertEvent' event) = V8.EditExpertEvent' <$> defaultUpgrade event
  upgrade (V7.DeleteExpertEvent' event) = V8.DeleteExpertEvent' <$> defaultUpgrade event
  upgrade (V7.AddReferenceEvent' event) = V8.AddReferenceEvent' <$> defaultUpgrade event
  upgrade (V7.EditReferenceEvent' event) = V8.EditReferenceEvent' <$> defaultUpgrade event
  upgrade (V7.DeleteReferenceEvent' event) = V8.DeleteReferenceEvent' <$> defaultUpgrade event
  upgrade (V7.AddTagEvent' event) = V8.AddTagEvent' <$> defaultUpgrade event
  upgrade (V7.EditTagEvent' event) = V8.EditTagEvent' <$> defaultUpgrade event
  upgrade (V7.DeleteTagEvent' event) = V8.DeleteTagEvent' <$> defaultUpgrade event
  upgrade (V7.AddIntegrationEvent' event) = V8.AddIntegrationEvent' <$> defaultUpgrade event
  upgrade (V7.EditIntegrationEvent' event) = V8.EditIntegrationEvent' <$> defaultUpgrade event
  upgrade (V7.DeleteIntegrationEvent' event) = V8.DeleteIntegrationEvent' <$> defaultUpgrade event
  upgrade (V7.MoveQuestionEvent' event) = V8.MoveQuestionEvent' <$> defaultUpgrade event
  upgrade (V7.MoveAnswerEvent' event) = V8.MoveAnswerEvent' <$> defaultUpgrade event
  upgrade (V7.MoveChoiceEvent' event) = V8.MoveChoiceEvent' <$> defaultUpgrade event
  upgrade (V7.MoveExpertEvent' event) = V8.MoveExpertEvent' <$> defaultUpgrade event
  upgrade (V7.MoveReferenceEvent' event) = V8.MoveReferenceEvent' <$> defaultUpgrade event
  additional (V7.AddKnowledgeModelEvent' e1) (V8.AddKnowledgeModelEvent' e2) = additional e1 e2
  additional e1 e2 = defaultAdditional e1 e2

migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V7.Event)
  additionals <- additional oldEvent newEvent
  return $ toJSON (newEvent :: V8.Event) : additionals
