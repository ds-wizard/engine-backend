module Wizard.Metamodel.Event.Version7 where

import Control.Monad
import Data.Aeson
import GHC.Generics

import Wizard.Metamodel.Event.Version7.Answer
import Wizard.Metamodel.Event.Version7.Chapter
import Wizard.Metamodel.Event.Version7.Choice
import Wizard.Metamodel.Event.Version7.Common
import Wizard.Metamodel.Event.Version7.Expert
import Wizard.Metamodel.Event.Version7.Integration
import Wizard.Metamodel.Event.Version7.KnowledgeModel
import Wizard.Metamodel.Event.Version7.Move
import Wizard.Metamodel.Event.Version7.Question
import Wizard.Metamodel.Event.Version7.Reference
import Wizard.Metamodel.Event.Version7.Tag

-- Created from engine-shared @7bdc1af98b8f3c210fa6562a9e7eba3fb7cdb507
-- Shared.Model.Event.Event
data Event
  = AddKnowledgeModelEvent' AddKnowledgeModelEvent
  | EditKnowledgeModelEvent' EditKnowledgeModelEvent
  | AddChapterEvent' AddChapterEvent
  | EditChapterEvent' EditChapterEvent
  | DeleteChapterEvent' DeleteChapterEvent
  | AddQuestionEvent' AddQuestionEvent
  | EditQuestionEvent' EditQuestionEvent
  | DeleteQuestionEvent' DeleteQuestionEvent
  | AddAnswerEvent' AddAnswerEvent
  | EditAnswerEvent' EditAnswerEvent
  | DeleteAnswerEvent' DeleteAnswerEvent
  | AddChoiceEvent' AddChoiceEvent
  | EditChoiceEvent' EditChoiceEvent
  | DeleteChoiceEvent' DeleteChoiceEvent
  | AddExpertEvent' AddExpertEvent
  | EditExpertEvent' EditExpertEvent
  | DeleteExpertEvent' DeleteExpertEvent
  | AddReferenceEvent' AddReferenceEvent
  | EditReferenceEvent' EditReferenceEvent
  | DeleteReferenceEvent' DeleteReferenceEvent
  | AddTagEvent' AddTagEvent
  | EditTagEvent' EditTagEvent
  | DeleteTagEvent' DeleteTagEvent
  | AddIntegrationEvent' AddIntegrationEvent
  | EditIntegrationEvent' EditIntegrationEvent
  | DeleteIntegrationEvent' DeleteIntegrationEvent
  | MoveQuestionEvent' MoveQuestionEvent
  | MoveAnswerEvent' MoveAnswerEvent
  | MoveChoiceEvent' MoveChoiceEvent
  | MoveExpertEvent' MoveExpertEvent
  | MoveReferenceEvent' MoveReferenceEvent
  deriving (Show, Eq, Generic)

-- EventJM
instance ToJSON Event where
  toJSON = toSumJSON' "eventType"

instance FromJSON Event where
  parseJSON (Object o) = do
    eventType <- o .: "eventType"
    case eventType of
      "AddKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (AddKnowledgeModelEvent' event)
      "EditKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (EditKnowledgeModelEvent' event)
      "AddChapterEvent" -> parseJSON (Object o) >>= \event -> return (AddChapterEvent' event)
      "EditChapterEvent" -> parseJSON (Object o) >>= \event -> return (EditChapterEvent' event)
      "DeleteChapterEvent" -> parseJSON (Object o) >>= \event -> return (DeleteChapterEvent' event)
      "AddQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddQuestionEvent' event)
      "EditQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditQuestionEvent' event)
      "DeleteQuestionEvent" -> parseJSON (Object o) >>= \event -> return (DeleteQuestionEvent' event)
      "AddAnswerEvent" -> parseJSON (Object o) >>= \event -> return (AddAnswerEvent' event)
      "EditAnswerEvent" -> parseJSON (Object o) >>= \event -> return (EditAnswerEvent' event)
      "DeleteAnswerEvent" -> parseJSON (Object o) >>= \event -> return (DeleteAnswerEvent' event)
      "AddChoiceEvent" -> parseJSON (Object o) >>= \event -> return (AddChoiceEvent' event)
      "EditChoiceEvent" -> parseJSON (Object o) >>= \event -> return (EditChoiceEvent' event)
      "DeleteChoiceEvent" -> parseJSON (Object o) >>= \event -> return (DeleteChoiceEvent' event)
      "AddExpertEvent" -> parseJSON (Object o) >>= \event -> return (AddExpertEvent' event)
      "EditExpertEvent" -> parseJSON (Object o) >>= \event -> return (EditExpertEvent' event)
      "DeleteExpertEvent" -> parseJSON (Object o) >>= \event -> return (DeleteExpertEvent' event)
      "AddReferenceEvent" -> parseJSON (Object o) >>= \event -> return (AddReferenceEvent' event)
      "EditReferenceEvent" -> parseJSON (Object o) >>= \event -> return (EditReferenceEvent' event)
      "DeleteReferenceEvent" -> parseJSON (Object o) >>= \event -> return (DeleteReferenceEvent' event)
      "AddTagEvent" -> parseJSON (Object o) >>= \event -> return (AddTagEvent' event)
      "EditTagEvent" -> parseJSON (Object o) >>= \event -> return (EditTagEvent' event)
      "DeleteTagEvent" -> parseJSON (Object o) >>= \event -> return (DeleteTagEvent' event)
      "AddIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (AddIntegrationEvent' event)
      "EditIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (EditIntegrationEvent' event)
      "DeleteIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (DeleteIntegrationEvent' event)
      "MoveQuestionEvent" -> parseJSON (Object o) >>= \event -> return (MoveQuestionEvent' event)
      "MoveAnswerEvent" -> parseJSON (Object o) >>= \event -> return (MoveAnswerEvent' event)
      "MoveChoiceEvent" -> parseJSON (Object o) >>= \event -> return (MoveChoiceEvent' event)
      "MoveExpertEvent" -> parseJSON (Object o) >>= \event -> return (MoveExpertEvent' event)
      "MoveReferenceEvent" -> parseJSON (Object o) >>= \event -> return (MoveReferenceEvent' event)
      _ -> fail "One of the events has unsupported eventType"
  parseJSON _ = mzero

