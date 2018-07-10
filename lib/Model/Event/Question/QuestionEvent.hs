module Model.Event.Question.QuestionEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath
import Model.KnowledgeModel.KnowledgeModel

data AddQuestionEvent = AddQuestionEvent
  { _addQuestionEventUuid :: UUID
  , _addQuestionEventPath :: EventPath
  , _addQuestionEventQuestionUuid :: UUID
  , _addQuestionEventQType :: QuestionType
  , _addQuestionEventTitle :: String
  , _addQuestionEventText :: String
  , _addQuestionEventAnswerItemTemplatePlain :: Maybe AnswerItemTemplatePlain
  } deriving (Show, Eq, Generic)

data EditQuestionEvent = EditQuestionEvent
  { _editQuestionEventUuid :: UUID
  , _editQuestionEventPath :: EventPath
  , _editQuestionEventQuestionUuid :: UUID
  , _editQuestionEventQType :: EventField QuestionType
  , _editQuestionEventTitle :: EventField String
  , _editQuestionEventText :: EventField String
  , _editQuestionEventAnswerItemTemplatePlainWithIds :: EventField (Maybe AnswerItemTemplatePlainWithIds)
  , _editQuestionEventAnswerIds :: EventField (Maybe [UUID])
  , _editQuestionEventExpertIds :: EventField [UUID]
  , _editQuestionEventReferenceIds :: EventField [UUID]
  } deriving (Show, Eq, Generic)

data DeleteQuestionEvent = DeleteQuestionEvent
  { _deleteQuestionEventUuid :: UUID
  , _deleteQuestionEventPath :: EventPath
  , _deleteQuestionEventQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)
