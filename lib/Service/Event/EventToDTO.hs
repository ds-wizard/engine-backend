module Service.Event.EventToDTO where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resource.Event.EventDTO
import Common.Types
import LensesConfig
import Model.Common
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.EventField
import Model.Event.Expert.ExpertEvent
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
import Model.Event.Reference.ReferenceEvent

-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
-- ------------------------------------------------------------------------
class EventToDTO a where
  toDTO :: a -> EventDTO

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
      , _editKnowledgeModelEventDTOName = event ^. name
      , _editKnowledgeModelEventDTOChapterIds = event ^. chapterIds
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
      , _editChapterEventDTOTitle = event ^. title
      , _editChapterEventDTOText = event ^. text
      , _editChapterEventDTOQuestionIds = event ^. questionIds
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
      }

instance EventToDTO EditQuestionEvent where
  toDTO event =
    EditQuestionEventDTO'
      EditQuestionEventDTO
      { _editQuestionEventDTOUuid = event ^. uuid
      , _editQuestionEventDTOKmUuid = event ^. kmUuid
      , _editQuestionEventDTOChapterUuid = event ^. chapterUuid
      , _editQuestionEventDTOQuestionUuid = event ^. questionUuid
      , _editQuestionEventDTOShortQuestionUuid = event ^. shortQuestionUuid
      , _editQuestionEventDTOQType = event ^. qType
      , _editQuestionEventDTOTitle = event ^. title
      , _editQuestionEventDTOText = event ^. text
      , _editQuestionEventDTOAnswerIds = event ^. answerIds
      , _editQuestionEventDTOExpertIds = event ^. expertIds
      , _editQuestionEventDTOReferenceIds = event ^. referenceIds
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
      , _editAnswerEventDTOLabel = event ^. label
      , _editAnswerEventDTOAdvice = event ^. advice
      , _editAnswerEventDTOFollowUpIds = event ^. followUpIds
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
      , _editExpertEventDTOName = event ^. name
      , _editExpertEventDTOEmail = event ^. email
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
      , _editReferenceEventDTOChapter = event ^. chapter
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
      , _editFollowUpQuestionEventDTOShortQuestionUuid = event ^. shortQuestionUuid
      , _editFollowUpQuestionEventDTOQType = event ^. qType
      , _editFollowUpQuestionEventDTOTitle = event ^. title
      , _editFollowUpQuestionEventDTOText = event ^. text
      , _editFollowUpQuestionEventDTOAnswerIds = event ^. answerIds
      , _editFollowUpQuestionEventDTOExpertIds = event ^. expertIds
      , _editFollowUpQuestionEventDTOReferenceIds = event ^. referenceIds
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
