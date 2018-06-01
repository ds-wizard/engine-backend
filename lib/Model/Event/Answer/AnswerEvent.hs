module Model.Event.Answer.AnswerEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath

data AddAnswerEvent = AddAnswerEvent
  { _addAnswerEventUuid :: UUID
  , _addAnswerEventPath :: EventPath
  , _addAnswerEventAnswerUuid :: UUID
  , _addAnswerEventLabel :: String
  , _addAnswerEventAdvice :: Maybe String
  } deriving (Show, Eq, Generic)

data EditAnswerEvent = EditAnswerEvent
  { _editAnswerEventUuid :: UUID
  , _editAnswerEventPath :: EventPath
  , _editAnswerEventAnswerUuid :: UUID
  , _editAnswerEventLabel :: EventField String
  , _editAnswerEventAdvice :: EventField (Maybe String)
  , _editAnswerEventFollowUpIds :: EventField [UUID]
  } deriving (Show, Eq, Generic)

data DeleteAnswerEvent = DeleteAnswerEvent
  { _deleteAnswerEventUuid :: UUID
  , _deleteAnswerEventPath :: EventPath
  , _deleteAnswerEventAnswerUuid :: UUID
  } deriving (Show, Eq, Generic)
