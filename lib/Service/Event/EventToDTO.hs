module Service.Event.EventToDTO where

import Control.Lens ((^.))

import Api.Resource.Event.EventDTO
import Api.Resource.Event.EventFieldDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import LensesConfig
import Model.Event.Answer.AnswerEvent
import Model.Event.AnswerItemTemplateQuestion.AnswerItemTemplateQuestionEvent
import Model.Event.Chapter.ChapterEvent
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
class EventToDTO a where
  toDTO :: a -> EventDTO

toEventFieldDTO :: EventField a -> EventFieldDTO a
toEventFieldDTO (ChangedValue value) = ChangedValueDTO value
toEventFieldDTO NothingChanged = NothingChangedDTO

toEventFieldAndAnswerItemTemplate ::
     EventField (Maybe AnswerItemTemplate) -> EventFieldDTO (Maybe AnswerItemTemplateDTO)
toEventFieldAndAnswerItemTemplate efMaybeAit =
  case efMaybeAit of
    ChangedValue maybeAit -> ChangedValueDTO $ toAnswerItemTemplateDTO <$> maybeAit
    NothingChanged -> NothingChangedDTO

toEventFieldAndAnswerItemTemplatePlain ::
     EventField (Maybe AnswerItemTemplatePlain) -> EventFieldDTO (Maybe AnswerItemTemplatePlainDTO)
toEventFieldAndAnswerItemTemplatePlain efMaybeAit =
  case efMaybeAit of
    ChangedValue maybeAit -> ChangedValueDTO $ toAnswerItemTemplatePlainDTO <$> maybeAit
    NothingChanged -> NothingChangedDTO

toEventFieldAndAnswerItemTemplatePlainWithIds ::
     EventField (Maybe AnswerItemTemplatePlainWithIds) -> EventFieldDTO (Maybe AnswerItemTemplatePlainWithIdsDTO)
toEventFieldAndAnswerItemTemplatePlainWithIds efMaybeAit =
  case efMaybeAit of
    ChangedValue maybeAit -> ChangedValueDTO $ toAnswerItemTemplatePlainWithIdsDTO <$> maybeAit
    NothingChanged -> NothingChangedDTO

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance EventToDTO AddKnowledgeModelEvent where
  toDTO event =
    AddKnowledgeModelEventDTO'
      AddKnowledgeModelEventDTO
      { _addKnowledgeModelEventDTOUuid = event ^. uuid
      , _addKnowledgeModelEventDTOKmUuid = event ^. kmUuid
      , _addKnowledgeModelEventDTOName = event ^. name
      }

instance EventToDTO EditKnowledgeModelEvent where
  toDTO event =
    EditKnowledgeModelEventDTO'
      EditKnowledgeModelEventDTO
      { _editKnowledgeModelEventDTOUuid = event ^. uuid
      , _editKnowledgeModelEventDTOKmUuid = event ^. kmUuid
      , _editKnowledgeModelEventDTOName = toEventFieldDTO $ event ^. name
      , _editKnowledgeModelEventDTOChapterIds = toEventFieldDTO $ event ^. chapterIds
      }

-------------------------
-- Chapter -----------------
-------------------------
instance EventToDTO AddChapterEvent where
  toDTO event =
    AddChapterEventDTO'
      AddChapterEventDTO
      { _addChapterEventDTOUuid = event ^. uuid
      , _addChapterEventDTOKmUuid = event ^. kmUuid
      , _addChapterEventDTOChapterUuid = event ^. chapterUuid
      , _addChapterEventDTOTitle = event ^. title
      , _addChapterEventDTOText = event ^. text
      }

instance EventToDTO EditChapterEvent where
  toDTO event =
    EditChapterEventDTO'
      EditChapterEventDTO
      { _editChapterEventDTOUuid = event ^. uuid
      , _editChapterEventDTOKmUuid = event ^. kmUuid
      , _editChapterEventDTOChapterUuid = event ^. chapterUuid
      , _editChapterEventDTOTitle = toEventFieldDTO $ event ^. title
      , _editChapterEventDTOText = toEventFieldDTO $ event ^. text
      , _editChapterEventDTOQuestionIds = toEventFieldDTO $ event ^. questionIds
      }

instance EventToDTO DeleteChapterEvent where
  toDTO event =
    DeleteChapterEventDTO'
      DeleteChapterEventDTO
      { _deleteChapterEventDTOUuid = event ^. uuid
      , _deleteChapterEventDTOKmUuid = event ^. kmUuid
      , _deleteChapterEventDTOChapterUuid = event ^. chapterUuid
      }

-- -------------------------
-- Question ----------------
-- -------------------------
instance EventToDTO AddQuestionEvent where
  toDTO event =
    AddQuestionEventDTO'
      AddQuestionEventDTO
      { _addQuestionEventDTOUuid = event ^. uuid
      , _addQuestionEventDTOKmUuid = event ^. kmUuid
      , _addQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _addQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _addQuestionEventDTOShortQuestionUuid = event ^. shortQuestionUuid
      , _addQuestionEventDTOQType = event ^. qType
      , _addQuestionEventDTOTitle = event ^. title
      , _addQuestionEventDTOText = event ^. text
      , _addQuestionEventDTOAnswerItemTemplatePlain = toAnswerItemTemplatePlainDTO <$> event ^. answerItemTemplatePlain
      }

instance EventToDTO EditQuestionEvent where
  toDTO event =
    EditQuestionEventDTO'
      EditQuestionEventDTO
      { _editQuestionEventDTOUuid = event ^. uuid
      , _editQuestionEventDTOKmUuid = event ^. kmUuid
      , _editQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _editQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _editQuestionEventDTOShortQuestionUuid = toEventFieldDTO $ event ^. shortQuestionUuid
      , _editQuestionEventDTOQType = toEventFieldDTO $ event ^. qType
      , _editQuestionEventDTOTitle = toEventFieldDTO $ event ^. title
      , _editQuestionEventDTOText = toEventFieldDTO $ event ^. text
      , _editQuestionEventDTOAnswerItemTemplatePlainWithIds =
          toEventFieldAndAnswerItemTemplatePlainWithIds $ event ^. answerItemTemplatePlainWithIds
      , _editQuestionEventDTOAnswerIds = toEventFieldDTO $ event ^. answerIds
      , _editQuestionEventDTOExpertIds = toEventFieldDTO $ event ^. expertIds
      , _editQuestionEventDTOReferenceIds = toEventFieldDTO $ event ^. referenceIds
      }

instance EventToDTO DeleteQuestionEvent where
  toDTO event =
    DeleteQuestionEventDTO'
      DeleteQuestionEventDTO
      { _deleteQuestionEventDTOUuid = event ^. uuid
      , _deleteQuestionEventDTOKmUuid = event ^. kmUuid
      , _deleteQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _deleteQuestionEventDTOQuestionUuid = event ^. questionUuid
      }

-- -------------------------
-- Answer ------------------
-- -------------------------
instance EventToDTO AddAnswerEvent where
  toDTO event =
    AddAnswerEventDTO'
      AddAnswerEventDTO
      { _addAnswerEventDTOUuid = event ^. uuid
      , _addAnswerEventDTOKmUuid = event ^. kmUuid
      , _addAnswerEventDTOChapterUuid = event ^. chapterUuid
      , _addAnswerEventDTOQuestionUuid = event ^. questionUuid
      , _addAnswerEventDTOAnswerUuid = event ^. answerUuid
      , _addAnswerEventDTOLabel = event ^. label
      , _addAnswerEventDTOAdvice = event ^. advice
      }

instance EventToDTO EditAnswerEvent where
  toDTO event =
    EditAnswerEventDTO'
      EditAnswerEventDTO
      { _editAnswerEventDTOUuid = event ^. uuid
      , _editAnswerEventDTOKmUuid = event ^. kmUuid
      , _editAnswerEventDTOChapterUuid = event ^. chapterUuid
      , _editAnswerEventDTOQuestionUuid = event ^. questionUuid
      , _editAnswerEventDTOAnswerUuid = event ^. answerUuid
      , _editAnswerEventDTOLabel = toEventFieldDTO $ event ^. label
      , _editAnswerEventDTOAdvice = toEventFieldDTO $ event ^. advice
      , _editAnswerEventDTOFollowUpIds = toEventFieldDTO $ event ^. followUpIds
      }

instance EventToDTO DeleteAnswerEvent where
  toDTO event =
    DeleteAnswerEventDTO'
      DeleteAnswerEventDTO
      { _deleteAnswerEventDTOUuid = event ^. uuid
      , _deleteAnswerEventDTOKmUuid = event ^. kmUuid
      , _deleteAnswerEventDTOChapterUuid = event ^. chapterUuid
      , _deleteAnswerEventDTOQuestionUuid = event ^. questionUuid
      , _deleteAnswerEventDTOAnswerUuid = event ^. answerUuid
      }

-- ---------------------------------
-- Answer Item Template Question ---
-- ---------------------------------
instance EventToDTO AddAnswerItemTemplateQuestionEvent where
  toDTO event =
    AddAnswerItemTemplateQuestionEventDTO'
      AddAnswerItemTemplateQuestionEventDTO
      { _addAnswerItemTemplateQuestionEventDTOUuid = event ^. uuid
      , _addAnswerItemTemplateQuestionEventDTOKmUuid = event ^. kmUuid
      , _addAnswerItemTemplateQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _addAnswerItemTemplateQuestionEventDTOParentQuestionUuid = event ^. parentQuestionUuid
      , _addAnswerItemTemplateQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _addAnswerItemTemplateQuestionEventDTOShortQuestionUuid = event ^. shortQuestionUuid
      , _addAnswerItemTemplateQuestionEventDTOQType = event ^. qType
      , _addAnswerItemTemplateQuestionEventDTOTitle = event ^. title
      , _addAnswerItemTemplateQuestionEventDTOText = event ^. text
      , _addAnswerItemTemplateQuestionEventDTOAnswerItemTemplatePlain =
          toAnswerItemTemplatePlainDTO <$> event ^. answerItemTemplatePlain
      }

instance EventToDTO EditAnswerItemTemplateQuestionEvent where
  toDTO event =
    EditAnswerItemTemplateQuestionEventDTO'
      EditAnswerItemTemplateQuestionEventDTO
      { _editAnswerItemTemplateQuestionEventDTOUuid = event ^. uuid
      , _editAnswerItemTemplateQuestionEventDTOKmUuid = event ^. kmUuid
      , _editAnswerItemTemplateQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _editAnswerItemTemplateQuestionEventDTOParentQuestionUuid = event ^. parentQuestionUuid
      , _editAnswerItemTemplateQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _editAnswerItemTemplateQuestionEventDTOShortQuestionUuid = toEventFieldDTO $ event ^. shortQuestionUuid
      , _editAnswerItemTemplateQuestionEventDTOQType = toEventFieldDTO $ event ^. qType
      , _editAnswerItemTemplateQuestionEventDTOTitle = toEventFieldDTO $ event ^. title
      , _editAnswerItemTemplateQuestionEventDTOText = toEventFieldDTO $ event ^. text
      , _editAnswerItemTemplateQuestionEventDTOAnswerItemTemplatePlainWithIds =
          toEventFieldAndAnswerItemTemplatePlainWithIds $ event ^. answerItemTemplatePlainWithIds
      , _editAnswerItemTemplateQuestionEventDTOAnswerIds = toEventFieldDTO $ event ^. answerIds
      , _editAnswerItemTemplateQuestionEventDTOExpertIds = toEventFieldDTO $ event ^. expertIds
      , _editAnswerItemTemplateQuestionEventDTOReferenceIds = toEventFieldDTO $ event ^. referenceIds
      }

instance EventToDTO DeleteAnswerItemTemplateQuestionEvent where
  toDTO event =
    DeleteAnswerItemTemplateQuestionEventDTO'
      DeleteAnswerItemTemplateQuestionEventDTO
      { _deleteAnswerItemTemplateQuestionEventDTOUuid = event ^. uuid
      , _deleteAnswerItemTemplateQuestionEventDTOKmUuid = event ^. kmUuid
      , _deleteAnswerItemTemplateQuestionEventDTOParentQuestionUuid = event ^. parentQuestionUuid
      , _deleteAnswerItemTemplateQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _deleteAnswerItemTemplateQuestionEventDTOQuestionUuid = event ^. questionUuid
      }

-- -------------------------
-- Expert ------------------
-- -------------------------
instance EventToDTO AddExpertEvent where
  toDTO event =
    AddExpertEventDTO'
      AddExpertEventDTO
      { _addExpertEventDTOUuid = event ^. uuid
      , _addExpertEventDTOKmUuid = event ^. kmUuid
      , _addExpertEventDTOChapterUuid = event ^. chapterUuid
      , _addExpertEventDTOQuestionUuid = event ^. questionUuid
      , _addExpertEventDTOExpertUuid = event ^. expertUuid
      , _addExpertEventDTOName = event ^. name
      , _addExpertEventDTOEmail = event ^. email
      }

instance EventToDTO EditExpertEvent where
  toDTO event =
    EditExpertEventDTO'
      EditExpertEventDTO
      { _editExpertEventDTOUuid = event ^. uuid
      , _editExpertEventDTOKmUuid = event ^. kmUuid
      , _editExpertEventDTOChapterUuid = event ^. chapterUuid
      , _editExpertEventDTOQuestionUuid = event ^. questionUuid
      , _editExpertEventDTOExpertUuid = event ^. expertUuid
      , _editExpertEventDTOName = toEventFieldDTO $ event ^. name
      , _editExpertEventDTOEmail = toEventFieldDTO $ event ^. email
      }

instance EventToDTO DeleteExpertEvent where
  toDTO event =
    DeleteExpertEventDTO'
      DeleteExpertEventDTO
      { _deleteExpertEventDTOUuid = event ^. uuid
      , _deleteExpertEventDTOKmUuid = event ^. kmUuid
      , _deleteExpertEventDTOChapterUuid = event ^. chapterUuid
      , _deleteExpertEventDTOQuestionUuid = event ^. questionUuid
      , _deleteExpertEventDTOExpertUuid = event ^. expertUuid
      }

-- -------------------------
-- Reference ---------------
-- -------------------------
instance EventToDTO AddReferenceEvent where
  toDTO event =
    AddReferenceEventDTO'
      AddReferenceEventDTO
      { _addReferenceEventDTOUuid = event ^. uuid
      , _addReferenceEventDTOKmUuid = event ^. kmUuid
      , _addReferenceEventDTOChapterUuid = event ^. chapterUuid
      , _addReferenceEventDTOQuestionUuid = event ^. questionUuid
      , _addReferenceEventDTOReferenceUuid = event ^. referenceUuid
      , _addReferenceEventDTOChapter = event ^. chapter
      }

instance EventToDTO EditReferenceEvent where
  toDTO event =
    EditReferenceEventDTO'
      EditReferenceEventDTO
      { _editReferenceEventDTOUuid = event ^. uuid
      , _editReferenceEventDTOKmUuid = event ^. kmUuid
      , _editReferenceEventDTOChapterUuid = event ^. chapterUuid
      , _editReferenceEventDTOQuestionUuid = event ^. questionUuid
      , _editReferenceEventDTOReferenceUuid = event ^. referenceUuid
      , _editReferenceEventDTOChapter = toEventFieldDTO $ event ^. chapter
      }

instance EventToDTO DeleteReferenceEvent where
  toDTO event =
    DeleteReferenceEventDTO'
      DeleteReferenceEventDTO
      { _deleteReferenceEventDTOUuid = event ^. uuid
      , _deleteReferenceEventDTOKmUuid = event ^. kmUuid
      , _deleteReferenceEventDTOChapterUuid = event ^. chapterUuid
      , _deleteReferenceEventDTOQuestionUuid = event ^. questionUuid
      , _deleteReferenceEventDTOReferenceUuid = event ^. referenceUuid
      }

-- -------------------------
-- Follow up question ------
-- -------------------------
instance EventToDTO AddFollowUpQuestionEvent where
  toDTO event =
    AddFollowUpQuestionEventDTO'
      AddFollowUpQuestionEventDTO
      { _addFollowUpQuestionEventDTOUuid = event ^. uuid
      , _addFollowUpQuestionEventDTOKmUuid = event ^. kmUuid
      , _addFollowUpQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _addFollowUpQuestionEventDTOAnswerUuid = event ^. answerUuid
      , _addFollowUpQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _addFollowUpQuestionEventDTOShortQuestionUuid = event ^. shortQuestionUuid
      , _addFollowUpQuestionEventDTOQType = event ^. qType
      , _addFollowUpQuestionEventDTOTitle = event ^. title
      , _addFollowUpQuestionEventDTOText = event ^. text
      , _addFollowUpQuestionEventDTOAnswerItemTemplatePlain =
          toAnswerItemTemplatePlainDTO <$> event ^. answerItemTemplatePlain
      }

instance EventToDTO EditFollowUpQuestionEvent where
  toDTO event =
    EditFollowUpQuestionEventDTO'
      EditFollowUpQuestionEventDTO
      { _editFollowUpQuestionEventDTOUuid = event ^. uuid
      , _editFollowUpQuestionEventDTOKmUuid = event ^. kmUuid
      , _editFollowUpQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _editFollowUpQuestionEventDTOAnswerUuid = event ^. answerUuid
      , _editFollowUpQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _editFollowUpQuestionEventDTOShortQuestionUuid = toEventFieldDTO $ event ^. shortQuestionUuid
      , _editFollowUpQuestionEventDTOQType = toEventFieldDTO $ event ^. qType
      , _editFollowUpQuestionEventDTOTitle = toEventFieldDTO $ event ^. title
      , _editFollowUpQuestionEventDTOText = toEventFieldDTO $ event ^. text
      , _editFollowUpQuestionEventDTOAnswerItemTemplatePlainWithIds =
          toEventFieldAndAnswerItemTemplatePlainWithIds $ event ^. answerItemTemplatePlainWithIds
      , _editFollowUpQuestionEventDTOAnswerIds = toEventFieldDTO $ event ^. answerIds
      , _editFollowUpQuestionEventDTOExpertIds = toEventFieldDTO $ event ^. expertIds
      , _editFollowUpQuestionEventDTOReferenceIds = toEventFieldDTO $ event ^. referenceIds
      }

instance EventToDTO DeleteFollowUpQuestionEvent where
  toDTO event =
    DeleteFollowUpQuestionEventDTO'
      DeleteFollowUpQuestionEventDTO
      { _deleteFollowUpQuestionEventDTOUuid = event ^. uuid
      , _deleteFollowUpQuestionEventDTOKmUuid = event ^. kmUuid
      , _deleteFollowUpQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _deleteFollowUpQuestionEventDTOAnswerUuid = event ^. answerUuid
      , _deleteFollowUpQuestionEventDTOQuestionUuid = event ^. questionUuid
      }
