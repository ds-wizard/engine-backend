module Service.Event.EventFromDTO where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resource.Event.EventDTO
import Api.Resource.Event.EventFieldDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Common.Types
import LensesConfig
import Model.Common
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Event
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent
import Model.KnowledgeModel.KnowledgeModel
import Service.KnowledgeModel.KnowledgeModelMapper

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class EventFromDTO a where
  fromDTO :: a -> Event

fromEventFieldDTO :: EventFieldDTO a -> EventField a
fromEventFieldDTO (ChangedValueDTO value) = ChangedValue value
fromEventFieldDTO NothingChangedDTO = NothingChanged

fromEventFieldAndAnswerItemTemplate ::
     EventFieldDTO (Maybe AnswerItemTemplateDTO) -> EventField (Maybe AnswerItemTemplate)
fromEventFieldAndAnswerItemTemplate efMaybeAitDto =
  case efMaybeAitDto of
    ChangedValueDTO maybeAitDto -> ChangedValue $ fromAnswerItemTemplateDTO <$> maybeAitDto
    NothingChangedDTO -> NothingChanged

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance EventFromDTO AddKnowledgeModelEventDTO where
  fromDTO dto =
    AddKnowledgeModelEvent'
      AddKnowledgeModelEvent
      { _addKnowledgeModelEventUuid = dto ^. uuid
      , _addKnowledgeModelEventKmUuid = dto ^. kmUuid
      , _addKnowledgeModelEventName = dto ^. name
      }

instance EventFromDTO EditKnowledgeModelEventDTO where
  fromDTO dto =
    EditKnowledgeModelEvent'
      EditKnowledgeModelEvent
      { _editKnowledgeModelEventUuid = dto ^. uuid
      , _editKnowledgeModelEventKmUuid = dto ^. kmUuid
      , _editKnowledgeModelEventName = fromEventFieldDTO $ dto ^. name
      , _editKnowledgeModelEventChapterIds = fromEventFieldDTO $ dto ^. chapterIds
      }

-- -------------------------
-- Chapter -----------------
-- -------------------------
instance EventFromDTO AddChapterEventDTO where
  fromDTO dto =
    AddChapterEvent'
      AddChapterEvent
      { _addChapterEventUuid = dto ^. uuid
      , _addChapterEventKmUuid = dto ^. kmUuid
      , _addChapterEventChapterUuid = dto ^. chapterUuid
      , _addChapterEventTitle = dto ^. title
      , _addChapterEventText = dto ^. text
      }

instance EventFromDTO EditChapterEventDTO where
  fromDTO dto =
    EditChapterEvent'
      EditChapterEvent
      { _editChapterEventUuid = dto ^. uuid
      , _editChapterEventKmUuid = dto ^. kmUuid
      , _editChapterEventChapterUuid = dto ^. chapterUuid
      , _editChapterEventTitle = fromEventFieldDTO $ dto ^. title
      , _editChapterEventText = fromEventFieldDTO $ dto ^. text
      , _editChapterEventQuestionIds = fromEventFieldDTO $ dto ^. questionIds
      }

instance EventFromDTO DeleteChapterEventDTO where
  fromDTO dto =
    DeleteChapterEvent'
      DeleteChapterEvent
      { _deleteChapterEventUuid = dto ^. uuid
      , _deleteChapterEventKmUuid = dto ^. kmUuid
      , _deleteChapterEventChapterUuid = dto ^. chapterUuid
      }

-- -------------------------
-- Question ----------------
-- -------------------------
instance EventFromDTO AddQuestionEventDTO where
  fromDTO dto =
    AddQuestionEvent'
      AddQuestionEvent
      { _addQuestionEventUuid = dto ^. uuid
      , _addQuestionEventKmUuid = dto ^. kmUuid
      , _addQuestionEventChapterUuid = dto ^. chapterUuid
      , _addQuestionEventQuestionUuid = dto ^. questionUuid
      , _addQuestionEventShortQuestionUuid = dto ^. shortQuestionUuid
      , _addQuestionEventQType = dto ^. qType
      , _addQuestionEventTitle = dto ^. title
      , _addQuestionEventText = dto ^. text
      , _addQuestionEventAnswerItemTemplate = fromAnswerItemTemplateDTO <$> dto ^. answerItemTemplate
      }

instance EventFromDTO EditQuestionEventDTO where
  fromDTO dto =
    EditQuestionEvent'
      EditQuestionEvent
      { _editQuestionEventUuid = dto ^. uuid
      , _editQuestionEventKmUuid = dto ^. kmUuid
      , _editQuestionEventChapterUuid = dto ^. chapterUuid
      , _editQuestionEventQuestionUuid = dto ^. questionUuid
      , _editQuestionEventShortQuestionUuid = fromEventFieldDTO $ dto ^. shortQuestionUuid
      , _editQuestionEventQType = fromEventFieldDTO $ dto ^. qType
      , _editQuestionEventTitle = fromEventFieldDTO $ dto ^. title
      , _editQuestionEventText = fromEventFieldDTO $ dto ^. text
      , _editQuestionEventAnswerItemTemplate = fromEventFieldAndAnswerItemTemplate $ dto ^. answerItemTemplate
      , _editQuestionEventAnswerIds = fromEventFieldDTO $ dto ^. answerIds
      , _editQuestionEventExpertIds = fromEventFieldDTO $ dto ^. expertIds
      , _editQuestionEventReferenceIds = fromEventFieldDTO $ dto ^. referenceIds
      }

instance EventFromDTO DeleteQuestionEventDTO where
  fromDTO dto =
    DeleteQuestionEvent'
      DeleteQuestionEvent
      { _deleteQuestionEventUuid = dto ^. uuid
      , _deleteQuestionEventKmUuid = dto ^. kmUuid
      , _deleteQuestionEventChapterUuid = dto ^. chapterUuid
      , _deleteQuestionEventQuestionUuid = dto ^. questionUuid
      }

-- -------------------------
-- Answer ------------------
-- -------------------------
instance EventFromDTO AddAnswerEventDTO where
  fromDTO dto =
    AddAnswerEvent'
      AddAnswerEvent
      { _addAnswerEventUuid = dto ^. uuid
      , _addAnswerEventKmUuid = dto ^. kmUuid
      , _addAnswerEventChapterUuid = dto ^. chapterUuid
      , _addAnswerEventQuestionUuid = dto ^. questionUuid
      , _addAnswerEventAnswerUuid = dto ^. answerUuid
      , _addAnswerEventLabel = dto ^. label
      , _addAnswerEventAdvice = dto ^. advice
      }

instance EventFromDTO EditAnswerEventDTO where
  fromDTO dto =
    EditAnswerEvent'
      EditAnswerEvent
      { _editAnswerEventUuid = dto ^. uuid
      , _editAnswerEventKmUuid = dto ^. kmUuid
      , _editAnswerEventChapterUuid = dto ^. chapterUuid
      , _editAnswerEventQuestionUuid = dto ^. questionUuid
      , _editAnswerEventAnswerUuid = dto ^. answerUuid
      , _editAnswerEventLabel = fromEventFieldDTO $ dto ^. label
      , _editAnswerEventAdvice = fromEventFieldDTO $ dto ^. advice
      , _editAnswerEventFollowUpIds = fromEventFieldDTO $ dto ^. followUpIds
      }

instance EventFromDTO DeleteAnswerEventDTO where
  fromDTO dto =
    DeleteAnswerEvent'
      DeleteAnswerEvent
      { _deleteAnswerEventUuid = dto ^. uuid
      , _deleteAnswerEventKmUuid = dto ^. kmUuid
      , _deleteAnswerEventChapterUuid = dto ^. chapterUuid
      , _deleteAnswerEventQuestionUuid = dto ^. questionUuid
      , _deleteAnswerEventAnswerUuid = dto ^. answerUuid
      }

-- -------------------------
-- Expert ------------------
-- -------------------------
instance EventFromDTO AddExpertEventDTO where
  fromDTO dto =
    AddExpertEvent'
      AddExpertEvent
      { _addExpertEventUuid = dto ^. uuid
      , _addExpertEventKmUuid = dto ^. kmUuid
      , _addExpertEventChapterUuid = dto ^. chapterUuid
      , _addExpertEventQuestionUuid = dto ^. questionUuid
      , _addExpertEventExpertUuid = dto ^. expertUuid
      , _addExpertEventName = dto ^. name
      , _addExpertEventEmail = dto ^. email
      }

instance EventFromDTO EditExpertEventDTO where
  fromDTO dto =
    EditExpertEvent'
      EditExpertEvent
      { _editExpertEventUuid = dto ^. uuid
      , _editExpertEventKmUuid = dto ^. kmUuid
      , _editExpertEventChapterUuid = dto ^. chapterUuid
      , _editExpertEventQuestionUuid = dto ^. questionUuid
      , _editExpertEventExpertUuid = dto ^. expertUuid
      , _editExpertEventName = fromEventFieldDTO $ dto ^. name
      , _editExpertEventEmail = fromEventFieldDTO $ dto ^. email
      }

instance EventFromDTO DeleteExpertEventDTO where
  fromDTO dto =
    DeleteExpertEvent'
      DeleteExpertEvent
      { _deleteExpertEventUuid = dto ^. uuid
      , _deleteExpertEventKmUuid = dto ^. kmUuid
      , _deleteExpertEventChapterUuid = dto ^. chapterUuid
      , _deleteExpertEventQuestionUuid = dto ^. questionUuid
      , _deleteExpertEventExpertUuid = dto ^. expertUuid
      }

-- -------------------------
-- Reference ---------------
-- -------------------------
instance EventFromDTO AddReferenceEventDTO where
  fromDTO dto =
    AddReferenceEvent'
      AddReferenceEvent
      { _addReferenceEventUuid = dto ^. uuid
      , _addReferenceEventKmUuid = dto ^. kmUuid
      , _addReferenceEventChapterUuid = dto ^. chapterUuid
      , _addReferenceEventQuestionUuid = dto ^. questionUuid
      , _addReferenceEventReferenceUuid = dto ^. referenceUuid
      , _addReferenceEventChapter = dto ^. chapter
      }

instance EventFromDTO EditReferenceEventDTO where
  fromDTO dto =
    EditReferenceEvent'
      EditReferenceEvent
      { _editReferenceEventUuid = dto ^. uuid
      , _editReferenceEventKmUuid = dto ^. kmUuid
      , _editReferenceEventChapterUuid = dto ^. chapterUuid
      , _editReferenceEventQuestionUuid = dto ^. questionUuid
      , _editReferenceEventReferenceUuid = dto ^. referenceUuid
      , _editReferenceEventChapter = fromEventFieldDTO $ dto ^. chapter
      }

instance EventFromDTO DeleteReferenceEventDTO where
  fromDTO dto =
    DeleteReferenceEvent'
      DeleteReferenceEvent
      { _deleteReferenceEventUuid = dto ^. uuid
      , _deleteReferenceEventKmUuid = dto ^. kmUuid
      , _deleteReferenceEventChapterUuid = dto ^. chapterUuid
      , _deleteReferenceEventQuestionUuid = dto ^. questionUuid
      , _deleteReferenceEventReferenceUuid = dto ^. referenceUuid
      }

-- -------------------------
-- Follow up question ------
-- -------------------------
instance EventFromDTO AddFollowUpQuestionEventDTO where
  fromDTO dto =
    AddFollowUpQuestionEvent'
      AddFollowUpQuestionEvent
      { _addFollowUpQuestionEventUuid = dto ^. uuid
      , _addFollowUpQuestionEventKmUuid = dto ^. kmUuid
      , _addFollowUpQuestionEventChapterUuid = dto ^. chapterUuid
      , _addFollowUpQuestionEventAnswerUuid = dto ^. answerUuid
      , _addFollowUpQuestionEventQuestionUuid = dto ^. questionUuid
      , _addFollowUpQuestionEventShortQuestionUuid = dto ^. shortQuestionUuid
      , _addFollowUpQuestionEventQType = dto ^. qType
      , _addFollowUpQuestionEventTitle = dto ^. title
      , _addFollowUpQuestionEventText = dto ^. text
      , _addFollowUpQuestionEventAnswerItemTemplate = fromAnswerItemTemplateDTO <$> dto ^. answerItemTemplate
      }

instance EventFromDTO EditFollowUpQuestionEventDTO where
  fromDTO dto =
    EditFollowUpQuestionEvent'
      EditFollowUpQuestionEvent
      { _editFollowUpQuestionEventUuid = dto ^. uuid
      , _editFollowUpQuestionEventKmUuid = dto ^. kmUuid
      , _editFollowUpQuestionEventChapterUuid = dto ^. chapterUuid
      , _editFollowUpQuestionEventAnswerUuid = dto ^. answerUuid
      , _editFollowUpQuestionEventQuestionUuid = dto ^. questionUuid
      , _editFollowUpQuestionEventShortQuestionUuid = fromEventFieldDTO $ dto ^. shortQuestionUuid
      , _editFollowUpQuestionEventQType = fromEventFieldDTO $ dto ^. qType
      , _editFollowUpQuestionEventTitle = fromEventFieldDTO $ dto ^. title
      , _editFollowUpQuestionEventText = fromEventFieldDTO $ dto ^. text
      , _editFollowUpQuestionEventAnswerItemTemplate = fromEventFieldAndAnswerItemTemplate $ dto ^. answerItemTemplate
      , _editFollowUpQuestionEventAnswerIds = fromEventFieldDTO $ dto ^. answerIds
      , _editFollowUpQuestionEventExpertIds = fromEventFieldDTO $ dto ^. expertIds
      , _editFollowUpQuestionEventReferenceIds = fromEventFieldDTO $ dto ^. referenceIds
      }

instance EventFromDTO DeleteFollowUpQuestionEventDTO where
  fromDTO dto =
    DeleteFollowUpQuestionEvent'
      DeleteFollowUpQuestionEvent
      { _deleteFollowUpQuestionEventUuid = dto ^. uuid
      , _deleteFollowUpQuestionEventKmUuid = dto ^. kmUuid
      , _deleteFollowUpQuestionEventChapterUuid = dto ^. chapterUuid
      , _deleteFollowUpQuestionEventAnswerUuid = dto ^. answerUuid
      , _deleteFollowUpQuestionEventQuestionUuid = dto ^. questionUuid
      }
