module Model.Event.FollowUpQuestion.FollowUpQuestionEvent where

import Data.UUID
import GHC.Generics

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
  } deriving (Show, Eq, Generic)

data EditFollowUpQuestionEvent = EditFollowUpQuestionEvent
  { _editFollowUpQuestionEventUuid :: UUID
  , _editFollowUpQuestionEventKmUuid :: UUID
  , _editFollowUpQuestionEventChapterUuid :: UUID
  , _editFollowUpQuestionEventAnswerUuid :: UUID
  , _editFollowUpQuestionEventQuestionUuid :: UUID
  , _editFollowUpQuestionEventShortQuestionUuid :: Maybe (Maybe String)
  , _editFollowUpQuestionEventQType :: Maybe QuestionType
  , _editFollowUpQuestionEventTitle :: Maybe String
  , _editFollowUpQuestionEventText :: Maybe String
  , _editFollowUpQuestionEventAnswerIds :: Maybe [UUID]
  , _editFollowUpQuestionEventExpertIds :: Maybe [UUID]
  , _editFollowUpQuestionEventReferenceIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

data DeleteFollowUpQuestionEvent = DeleteFollowUpQuestionEvent
  { _deleteFollowUpQuestionEventUuid :: UUID
  , _deleteFollowUpQuestionEventKmUuid :: UUID
  , _deleteFollowUpQuestionEventChapterUuid :: UUID
  , _deleteFollowUpQuestionEventAnswerUuid :: UUID
  , _deleteFollowUpQuestionEventQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)
