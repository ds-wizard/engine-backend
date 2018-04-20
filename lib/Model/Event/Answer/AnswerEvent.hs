module Model.Event.Answer.AnswerEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField

data AddAnswerEvent = AddAnswerEvent
  { _addAnswerEventUuid :: UUID
  , _addAnswerEventKmUuid :: UUID
  , _addAnswerEventChapterUuid :: UUID
  , _addAnswerEventQuestionUuid :: UUID
  , _addAnswerEventAnswerUuid :: UUID
  , _addAnswerEventLabel :: String
  , _addAnswerEventAdvice :: Maybe String
  } deriving (Show, Eq, Generic)

data EditAnswerEvent = EditAnswerEvent
  { _editAnswerEventUuid :: UUID
  , _editAnswerEventKmUuid :: UUID
  , _editAnswerEventChapterUuid :: UUID
  , _editAnswerEventQuestionUuid :: UUID
  , _editAnswerEventAnswerUuid :: UUID
  , _editAnswerEventLabel :: EventField String
  , _editAnswerEventAdvice :: EventField (Maybe String)
  , _editAnswerEventFollowUpIds :: EventField [UUID]
  } deriving (Show, Eq, Generic)

data DeleteAnswerEvent = DeleteAnswerEvent
  { _deleteAnswerEventUuid :: UUID
  , _deleteAnswerEventKmUuid :: UUID
  , _deleteAnswerEventChapterUuid :: UUID
  , _deleteAnswerEventQuestionUuid :: UUID
  , _deleteAnswerEventAnswerUuid :: UUID
  } deriving (Show, Eq, Generic)
