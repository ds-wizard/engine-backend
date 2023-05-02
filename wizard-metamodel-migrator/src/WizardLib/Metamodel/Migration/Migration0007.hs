module WizardLib.Metamodel.Migration.Migration0007 (
  migrateEventValue,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import WizardLib.Metamodel.Migration.MigrationContext
import WizardLib.Metamodel.Migration.Utils

-- Migration #0007 (KM v7 -> v8)

-- * Extend with default phases and metrics

-- * Change levels to phases in question events
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ = Right . migrate

mkPhaseEvent :: T.Text -> T.Text -> T.Text -> T.Text -> Value
mkPhaseEvent title eventUuid entityUuid parentUuid =
  Object $
    KM.fromList
      [ ("uuid", String eventUuid)
      , ("entityUuid", String entityUuid)
      , ("parentUuid", String parentUuid)
      , ("entityUuid", String entityUuid)
      , ("eventType", String "AddPhaseEvent")
      , ("title", String title)
      , ("description", Null)
      ]

mkMetricEvent :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> Value
mkMetricEvent title abbr description eventUuid entityUuid parentUuid =
  Object $
    KM.fromList
      [ ("uuid", String eventUuid)
      , ("entityUuid", String entityUuid)
      , ("parentUuid", String parentUuid)
      , ("entityUuid", String entityUuid)
      , ("eventType", String "AddMetricEvent")
      , ("title", String title)
      , ("abbreviation", String abbr)
      , ("description", String description)
      ]

uuidPhase1 :: T.Text
uuidPhase1 = "b101f2d0-2476-452d-aa8d-95a41a02b52c"

uuidPhase2 :: T.Text
uuidPhase2 = "1796fa3c-9f53-475f-89ff-c66a0453c42e"

uuidPhase3 :: T.Text
uuidPhase3 = "adc9133d-afcd-4616-9aea-db5f475898a2"

uuidPhase4 :: T.Text
uuidPhase4 = "1ace0fc6-a949-495f-a32e-e948f3f6bed1"

addPhase1 = mkPhaseEvent "Before Submitting the Proposal" "eff6bbfc-8983-4799-8b89-f7a4ea37b611" uuidPhase1

addPhase2 = mkPhaseEvent "Before Submitting the DMP" "e9bfcd64-411e-424a-98c9-5b4d531f0890" uuidPhase2

addPhase3 = mkPhaseEvent "Before Finishing the Project" "bd22e504-295a-4ade-8fca-7f86aacddd76" uuidPhase3

addPhase4 = mkPhaseEvent "After Finishing the Project" "fc62249c-700a-45d2-97b8-e5817468a5d5" uuidPhase4

addPhases :: T.Text -> [Value]
addPhases kmUuid = map ($ kmUuid) [addPhase1, addPhase2, addPhase3, addPhase4]

addMetricF =
  mkMetricEvent
    "Findability"
    "F"
    "The Findability metric describes how easily data can be located. The score associated with an answer will be higher if it makes it easier for humans or for computers to locate your data set, e.g. if it ends up in an index or has a unique resolvable identifier."
    "b630056c-3acf-4f26-b317-50f1805402c0"
    "8db30660-d4e5-4c0a-bf3e-553f3f0f997a"

addMetricA =
  mkMetricEvent
    "Accessibility"
    "A"
    "The Accessibility metric describes how well the access to the database is described and how easy it is to implement. The score associated with an answer will be higher if it makes it easier for humans and computers to get to the data. This is determined by e.g. the protocol for accessing the data or for authenticating users, and also by the guaranteed longevity of the repository. Note that this is different from the Openness metric!"
    "c19feec6-2ab2-4757-893a-c11c47f352b8"
    "0feac7e6-add4-4723-abae-be5ce7864c63"

addMetricI =
  mkMetricEvent
    "Interoperability"
    "I"
    "The Interoperability metric describes how well the data interoperates with other data. The score associated with an answer will be higher if it makes it easier for humans and computers to couple the data with other data and 'understand' relationships. This is influenced by the use of standard ontologies for different fields and proper descriptions of the relations. It is also influenced by proper standard metadata that is agreed by the community."
    "412854a9-ee08-4630-8e9a-71978c8290b3"
    "a42bded3-a085-45f8-b384-32b4a77c8385"

addMetricR =
  mkMetricEvent
    "Reusability"
    "R"
    "The Reusability metric describes how well the data is suitable for reuse in other context. The score associated with an answer will be higher if it makes it easier for humans and computers to reuse the data. This is influenced largely by proper description of how the data was obtained, and also by the conditions that are put on the reuse (license and, for personally identifying information, consent)."
    "b0bf9ac7-cd4c-4e77-9d56-2363e558bc8e"
    "0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7"

addMetricG =
  mkMetricEvent
    "Good DMP Practice"
    "G"
    "The Good DMP Practice metric describes how appreciated a process is among Data Stewards. A score associated with an answer will be high if a practice would be considered preferable over alternatives, generally a good idea."
    "c99e578d-f4cc-4a36-94cc-1451901f11fe"
    "8845fe2b-79df-4138-baea-3a035bf5e249"

addMetricO =
  mkMetricEvent
    "Openness"
    "O"
    "The Openness metric describes how Open the data are available. Note that this is different from the Accessibility metric. A score associated with an answer will be high if the data will be as open as possible, and low if voluntary restrictions apply to access and re-use."
    "04d738dd-3b71-4433-afd6-65b105fa71cd"
    "cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0"

addMetrics :: T.Text -> [Value]
addMetrics kmUuid = map ($ kmUuid) [addMetricF, addMetricA, addMetricI, addMetricR, addMetricG, addMetricO]

transformPhase :: Value -> Value
transformPhase (Number 1) = String uuidPhase1
transformPhase (Number 2) = String uuidPhase2
transformPhase (Number 3) = String uuidPhase3
transformPhase (Number 4) = String uuidPhase4
transformPhase _ = Null

migrateAddKnowledgeModelEvent :: Object -> [Value]
migrateAddKnowledgeModelEvent obj =
  case KM.lookup "entityUuid" obj of
    (Just (String kmUuid)) -> [Object obj] ++ addPhases kmUuid ++ addMetrics kmUuid
    _ -> [Object obj]

migrateEditKnowledgeModelEvent :: Object -> Object
migrateEditKnowledgeModelEvent = runBasicOps [Insert "metricUuids" unchangedValue, Insert "phaseUuids" unchangedValue]

migrateAddQuestionEvent :: Object -> Object
migrateAddQuestionEvent =
  runBasicOps [Rename "requiredLevel" "requiredPhaseUuid", Change "requiredPhaseUuid" transformPhase]

migrateEditQuestionEvent :: Object -> Object
migrateEditQuestionEvent =
  runBasicOps
    [Rename "requiredLevel" "requiredPhaseUuid", Change "requiredPhaseUuid" (applyOnEventField transformPhase)]

migrate :: Value -> [Value]
migrate v@(Object obj)
  | eventType == "AddKnowledgeModelEvent" = migrateAddKnowledgeModelEvent obj
  | eventType == "EditKnowledgeModelEvent" = [Object $ migrateEditKnowledgeModelEvent obj]
  | eventType == "AddQuestionEvent" = [Object $ migrateAddQuestionEvent obj]
  | eventType == "EditQuestionEvent" = [Object $ migrateEditQuestionEvent obj]
  | otherwise = [v]
  where
    eventType = fromMaybe "" . KM.lookup "eventType" $ obj
migrate v = [v]
