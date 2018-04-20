module Model.Event.Question.QuestionEvent where

import Data.UUID
import GHC.Generics

import Model.KnowledgeModel.KnowledgeModel

data AddQuestionEvent = AddQuestionEvent
  { _addQuestionEventUuid :: UUID
  , _addQuestionEventKmUuid :: UUID
  , _addQuestionEventChapterUuid :: UUID
  , _addQuestionEventQuestionUuid :: UUID
  , _addQuestionEventShortQuestionUuid :: Maybe String
  , _addQuestionEventQType :: QuestionType
  , _addQuestionEventTitle :: String
  , _addQuestionEventText :: String
  } deriving (Show, Eq, Generic)

data EditQuestionEvent = EditQuestionEvent
  { _editQuestionEventUuid :: UUID
  , _editQuestionEventKmUuid :: UUID
  , _editQuestionEventChapterUuid :: UUID
  , _editQuestionEventQuestionUuid :: UUID
  , _editQuestionEventShortQuestionUuid :: Maybe (Maybe String)
  , _editQuestionEventQType :: Maybe QuestionType
  , _editQuestionEventTitle :: Maybe String
  , _editQuestionEventText :: Maybe String
  , _editQuestionEventAnswerIds :: Maybe [UUID]
  , _editQuestionEventExpertIds :: Maybe [UUID]
  , _editQuestionEventReferenceIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

data DeleteQuestionEvent = DeleteQuestionEvent
  { _deleteQuestionEventUuid :: UUID
  , _deleteQuestionEventKmUuid :: UUID
  , _deleteQuestionEventChapterUuid :: UUID
  , _deleteQuestionEventQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)
