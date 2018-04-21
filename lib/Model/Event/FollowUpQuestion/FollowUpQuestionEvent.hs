module Model.Event.FollowUpQuestion.FollowUpQuestionEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField
import Model.KnowledgeModel.KnowledgeModel

data AddFollowUpQuestionEvent = AddFollowUpQuestionEvent
  { _addFollowUpQuestionEventUuid :: UUID
  , _addFollowUpQuestionEventKmUuid :: UUID
  , _addFollowUpQuestionEventChapterUuid :: UUID
  , _addFollowUpQuestionEventAnswerUuid :: UUID
  , _addFollowUpQuestionEventQuestionUuid :: UUID
  , _addFollowUpQuestionEventShortQuestionUuid :: Maybe String
  , _addFollowUpQuestionEventQType :: QuestionType
  , _addFollowUpQuestionEventTitle :: String
  , _addFollowUpQuestionEventText :: String
  , _addFollowUpQuestionEventAnswerItemTemplate :: Maybe AnswerItemTemplate
  } deriving (Show, Eq, Generic)

data EditFollowUpQuestionEvent = EditFollowUpQuestionEvent
  { _editFollowUpQuestionEventUuid :: UUID
  , _editFollowUpQuestionEventKmUuid :: UUID
  , _editFollowUpQuestionEventChapterUuid :: UUID
  , _editFollowUpQuestionEventAnswerUuid :: UUID
  , _editFollowUpQuestionEventQuestionUuid :: UUID
  , _editFollowUpQuestionEventShortQuestionUuid :: EventField (Maybe String)
  , _editFollowUpQuestionEventQType :: EventField QuestionType
  , _editFollowUpQuestionEventTitle :: EventField String
  , _editFollowUpQuestionEventText :: EventField String
  , _editFollowUpQuestionEventAnswerItemTemplate :: EventField (Maybe AnswerItemTemplate)
  , _editFollowUpQuestionEventAnswerIds :: EventField (Maybe [UUID])
  , _editFollowUpQuestionEventExpertIds :: EventField [UUID]
  , _editFollowUpQuestionEventReferenceIds :: EventField [UUID]
  } deriving (Show, Eq, Generic)

data DeleteFollowUpQuestionEvent = DeleteFollowUpQuestionEvent
  { _deleteFollowUpQuestionEventUuid :: UUID
  , _deleteFollowUpQuestionEventKmUuid :: UUID
  , _deleteFollowUpQuestionEventChapterUuid :: UUID
  , _deleteFollowUpQuestionEventAnswerUuid :: UUID
  , _deleteFollowUpQuestionEventQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)
