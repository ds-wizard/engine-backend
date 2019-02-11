module Model.Event.Question.QuestionEvent where

import qualified Data.UUID as U
import GHC.Generics

import Model.Event.EventField
import Model.Event.EventPath
import Model.KnowledgeModel.KnowledgeModel

data AddQuestionEvent = AddQuestionEvent
  { _addQuestionEventUuid :: U.UUID
  , _addQuestionEventPath :: EventPath
  , _addQuestionEventQuestionUuid :: U.UUID
  , _addQuestionEventQType :: QuestionType
  , _addQuestionEventTitle :: String
  , _addQuestionEventText :: Maybe String
  , _addQuestionEventRequiredLevel :: Maybe Int
  , _addQuestionEventTagUuids :: [U.UUID]
  , _addQuestionEventAnswerItemTemplatePlain :: Maybe AnswerItemTemplatePlain
  } deriving (Show, Eq, Generic)

data EditQuestionEvent = EditQuestionEvent
  { _editQuestionEventUuid :: U.UUID
  , _editQuestionEventPath :: EventPath
  , _editQuestionEventQuestionUuid :: U.UUID
  , _editQuestionEventQType :: EventField QuestionType
  , _editQuestionEventTitle :: EventField String
  , _editQuestionEventText :: EventField (Maybe String)
  , _editQuestionEventRequiredLevel :: EventField (Maybe Int)
  , _editQuestionEventTagUuids :: EventField [U.UUID]
  , _editQuestionEventAnswerItemTemplatePlainWithUuids :: EventField (Maybe AnswerItemTemplatePlainWithUuids)
  , _editQuestionEventAnswerUuids :: EventField (Maybe [U.UUID])
  , _editQuestionEventExpertUuids :: EventField [U.UUID]
  , _editQuestionEventReferenceUuids :: EventField [U.UUID]
  } deriving (Show, Eq, Generic)

data DeleteQuestionEvent = DeleteQuestionEvent
  { _deleteQuestionEventUuid :: U.UUID
  , _deleteQuestionEventPath :: EventPath
  , _deleteQuestionEventQuestionUuid :: U.UUID
  } deriving (Show, Eq, Generic)
