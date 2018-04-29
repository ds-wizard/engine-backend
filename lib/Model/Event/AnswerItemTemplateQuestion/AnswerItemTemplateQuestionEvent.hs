module Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEvent where

import Data.UUID
import GHC.Generics

import Model.Event.EventField
import Model.KnowledgeModel.KnowledgeModel

data AddAnswerItemTemplateQuestionEvent = AddAnswerItemTemplateQuestionEvent
  { _addAnswerItemTemplateQuestionEventUuid :: UUID
  , _addAnswerItemTemplateQuestionEventKmUuid :: UUID
  , _addAnswerItemTemplateQuestionEventChapterUuid :: UUID
  , _addAnswerItemTemplateQuestionEventParentQuestionUuid :: UUID
  , _addAnswerItemTemplateQuestionEventQuestionUuid :: UUID
  , _addAnswerItemTemplateQuestionEventShortQuestionUuid :: Maybe String
  , _addAnswerItemTemplateQuestionEventQType :: QuestionType
  , _addAnswerItemTemplateQuestionEventTitle :: String
  , _addAnswerItemTemplateQuestionEventText :: String
  , _addAnswerItemTemplateQuestionEventAnswerItemTemplatePlain :: Maybe AnswerItemTemplatePlain
  } deriving (Show, Eq, Generic)

data EditAnswerItemTemplateQuestionEvent = EditAnswerItemTemplateQuestionEvent
  { _editAnswerItemTemplateQuestionEventUuid :: UUID
  , _editAnswerItemTemplateQuestionEventKmUuid :: UUID
  , _editAnswerItemTemplateQuestionEventChapterUuid :: UUID
  , _editAnswerItemTemplateQuestionEventParentQuestionUuid :: UUID
  , _editAnswerItemTemplateQuestionEventQuestionUuid :: UUID
  , _editAnswerItemTemplateQuestionEventShortQuestionUuid :: EventField (Maybe String)
  , _editAnswerItemTemplateQuestionEventQType :: EventField QuestionType
  , _editAnswerItemTemplateQuestionEventTitle :: EventField String
  , _editAnswerItemTemplateQuestionEventText :: EventField String
  , _editAnswerItemTemplateQuestionEventAnswerItemTemplatePlainWithIds :: EventField (Maybe AnswerItemTemplatePlainWithIds)
  , _editAnswerItemTemplateQuestionEventAnswerIds :: EventField (Maybe [UUID])
  , _editAnswerItemTemplateQuestionEventExpertIds :: EventField [UUID]
  , _editAnswerItemTemplateQuestionEventReferenceIds :: EventField [UUID]
  } deriving (Show, Eq, Generic)

data DeleteAnswerItemTemplateQuestionEvent = DeleteAnswerItemTemplateQuestionEvent
  { _deleteAnswerItemTemplateQuestionEventUuid :: UUID
  , _deleteAnswerItemTemplateQuestionEventKmUuid :: UUID
  , _deleteAnswerItemTemplateQuestionEventChapterUuid :: UUID
  , _deleteAnswerItemTemplateQuestionEventParentQuestionUuid :: UUID
  , _deleteAnswerItemTemplateQuestionEventQuestionUuid :: UUID
  } deriving (Show, Eq, Generic)
