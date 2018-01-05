module Service.Event.EventToDTO where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resource.Event.EventDTO
import Common.Types
import Model.Common
import Model.Event.Answer.AddAnswerEvent
import Model.Event.Answer.DeleteAnswerEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Chapter.DeleteChapterEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Expert.AddExpertEvent
import Model.Event.Expert.DeleteExpertEvent
import Model.Event.Expert.EditExpertEvent
import Model.Event.FollowUpQuestion.AddFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.DeleteFollowUpQuestionEvent
import Model.Event.FollowUpQuestion.EditFollowUpQuestionEvent
import Model.Event.KnowledgeModel.AddKnowledgeModelEvent
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.Event.Question.AddQuestionEvent
import Model.Event.Question.DeleteQuestionEvent
import Model.Event.Question.EditQuestionEvent
import Model.Event.Reference.AddReferenceEvent
import Model.Event.Reference.DeleteReferenceEvent
import Model.Event.Reference.EditReferenceEvent

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
      {_akmdtoUuid = event ^. akmUuid, _akmdtoKmUuid = event ^. akmKmUuid, _akmdtoName = event ^. akmName}

instance EventToDTO EditKnowledgeModelEvent where
  toDTO event =
    EditKnowledgeModelEventDTO'
      EditKnowledgeModelEventDTO
      { _ekmdtoUuid = event ^. ekmUuid
      , _ekmdtoKmUuid = event ^. ekmKmUuid
      , _ekmdtoName = event ^. ekmName
      , _ekmdtoChapterIds = event ^. ekmChapterIds
      }

-------------------------
-- Chapter -----------------
-------------------------
instance EventToDTO AddChapterEvent where
  toDTO event =
    AddChapterEventDTO'
      AddChapterEventDTO
      { _achdtoUuid = event ^. achUuid
      , _achdtoKmUuid = event ^. achKmUuid
      , _achdtoChapterUuid = event ^. achChapterUuid
      , _achdtoTitle = event ^. achTitle
      , _achdtoText = event ^. achText
      }

instance EventToDTO EditChapterEvent where
  toDTO event =
    EditChapterEventDTO'
      EditChapterEventDTO
      { _echdtoUuid = event ^. echUuid
      , _echdtoKmUuid = event ^. echKmUuid
      , _echdtoChapterUuid = event ^. echChapterUuid
      , _echdtoTitle = event ^. echTitle
      , _echdtoText = event ^. echText
      , _echdtoQuestionIds = event ^. echQuestionIds
      }

instance EventToDTO DeleteChapterEvent where
  toDTO event =
    DeleteChapterEventDTO'
      DeleteChapterEventDTO
      {_dchdtoUuid = event ^. dchUuid, _dchdtoKmUuid = event ^. dchKmUuid, _dchdtoChapterUuid = event ^. dchChapterUuid}

-- -------------------------
-- Question ----------------
-- -------------------------
instance EventToDTO AddQuestionEvent where
  toDTO event =
    AddQuestionEventDTO'
      AddQuestionEventDTO
      { _aqdtoUuid = event ^. aqUuid
      , _aqdtoKmUuid = event ^. aqKmUuid
      , _aqdtoChapterUuid = event ^. aqChapterUuid
      , _aqdtoQuestionUuid = event ^. aqQuestionUuid
      , _aqdtoShortQuestionUuid = event ^. aqShortQuestionUuid
      , _aqdtoType = event ^. aqType
      , _aqdtoTitle = event ^. aqTitle
      , _aqdtoText = event ^. aqText
      }

instance EventToDTO EditQuestionEvent where
  toDTO event =
    EditQuestionEventDTO'
      EditQuestionEventDTO
      { _eqdtoUuid = event ^. eqUuid
      , _eqdtoKmUuid = event ^. eqKmUuid
      , _eqdtoChapterUuid = event ^. eqChapterUuid
      , _eqdtoQuestionUuid = event ^. eqQuestionUuid
      , _eqdtoShortQuestionUuid = event ^. eqShortQuestionUuid
      , _eqdtoType = event ^. eqType
      , _eqdtoTitle = event ^. eqTitle
      , _eqdtoText = event ^. eqText
      , _eqdtoAnswerIds = event ^. eqAnswerIds
      , _eqdtoExpertIds = event ^. eqExpertIds
      , _eqdtoReferenceIds = event ^. eqReferenceIds
      }

instance EventToDTO DeleteQuestionEvent where
  toDTO event =
    DeleteQuestionEventDTO'
      DeleteQuestionEventDTO
      { _dqdtoUuid = event ^. dqUuid
      , _dqdtoKmUuid = event ^. dqKmUuid
      , _dqdtoChapterUuid = event ^. dqChapterUuid
      , _dqdtoQuestionUuid = event ^. dqQuestionUuid
      }

-- -------------------------
-- Answer ------------------
-- -------------------------
instance EventToDTO AddAnswerEvent where
  toDTO event =
    AddAnswerEventDTO'
      AddAnswerEventDTO
      { _aansdtoUuid = event ^. aansUuid
      , _aansdtoKmUuid = event ^. aansKmUuid
      , _aansdtoChapterUuid = event ^. aansChapterUuid
      , _aansdtoQuestionUuid = event ^. aansQuestionUuid
      , _aansdtoAnswerUuid = event ^. aansAnswerUuid
      , _aansdtoLabel = event ^. aansLabel
      , _aansdtoAdvice = event ^. aansAdvice
      }

instance EventToDTO EditAnswerEvent where
  toDTO event =
    EditAnswerEventDTO'
      EditAnswerEventDTO
      { _eansdtoUuid = event ^. eansUuid
      , _eansdtoKmUuid = event ^. eansKmUuid
      , _eansdtoChapterUuid = event ^. eansChapterUuid
      , _eansdtoQuestionUuid = event ^. eansQuestionUuid
      , _eansdtoAnswerUuid = event ^. eansAnswerUuid
      , _eansdtoLabel = event ^. eansLabel
      , _eansdtoAdvice = event ^. eansAdvice
      , _eansdtoFollowUpIds = event ^. eansFollowUpIds
      }

instance EventToDTO DeleteAnswerEvent where
  toDTO event =
    DeleteAnswerEventDTO'
      DeleteAnswerEventDTO
      { _dansdtoUuid = event ^. dansUuid
      , _dansdtoKmUuid = event ^. dansKmUuid
      , _dansdtoChapterUuid = event ^. dansChapterUuid
      , _dansdtoQuestionUuid = event ^. dansQuestionUuid
      , _dansdtoAnswerUuid = event ^. dansAnswerUuid
      }

-- -------------------------
-- Expert ------------------
-- -------------------------
instance EventToDTO AddExpertEvent where
  toDTO event =
    AddExpertEventDTO'
      AddExpertEventDTO
      { _aexpdtoUuid = event ^. aexpUuid
      , _aexpdtoKmUuid = event ^. aexpKmUuid
      , _aexpdtoChapterUuid = event ^. aexpChapterUuid
      , _aexpdtoQuestionUuid = event ^. aexpQuestionUuid
      , _aexpdtoExpertUuid = event ^. aexpExpertUuid
      , _aexpdtoName = event ^. aexpName
      , _aexpdtoEmail = event ^. aexpEmail
      }

instance EventToDTO EditExpertEvent where
  toDTO event =
    EditExpertEventDTO'
      EditExpertEventDTO
      { _eexpdtoUuid = event ^. eexpUuid
      , _eexpdtoKmUuid = event ^. eexpKmUuid
      , _eexpdtoChapterUuid = event ^. eexpChapterUuid
      , _eexpdtoQuestionUuid = event ^. eexpQuestionUuid
      , _eexpdtoExpertUuid = event ^. eexpExpertUuid
      , _eexpdtoName = event ^. eexpName
      , _eexpdtoEmail = event ^. eexpEmail
      }

instance EventToDTO DeleteExpertEvent where
  toDTO event =
    DeleteExpertEventDTO'
      DeleteExpertEventDTO
      { _dexpdtoUuid = event ^. dexpUuid
      , _dexpdtoKmUuid = event ^. dexpKmUuid
      , _dexpdtoChapterUuid = event ^. dexpChapterUuid
      , _dexpdtoQuestionUuid = event ^. dexpQuestionUuid
      , _dexpdtoExpertUuid = event ^. dexpExpertUuid
      }

-- -------------------------
-- Reference ---------------
-- -------------------------
instance EventToDTO AddReferenceEvent where
  toDTO event =
    AddReferenceEventDTO'
      AddReferenceEventDTO
      { _arefdtoUuid = event ^. arefUuid
      , _arefdtoKmUuid = event ^. arefKmUuid
      , _arefdtoChapterUuid = event ^. arefChapterUuid
      , _arefdtoQuestionUuid = event ^. arefQuestionUuid
      , _arefdtoReferenceUuid = event ^. arefReferenceUuid
      , _arefdtoChapter = event ^. arefChapter
      }

instance EventToDTO EditReferenceEvent where
  toDTO event =
    EditReferenceEventDTO'
      EditReferenceEventDTO
      { _erefdtoUuid = event ^. erefUuid
      , _erefdtoKmUuid = event ^. erefKmUuid
      , _erefdtoChapterUuid = event ^. erefChapterUuid
      , _erefdtoQuestionUuid = event ^. erefQuestionUuid
      , _erefdtoReferenceUuid = event ^. erefReferenceUuid
      , _erefdtoChapter = event ^. erefChapter
      }

instance EventToDTO DeleteReferenceEvent where
  toDTO event =
    DeleteReferenceEventDTO'
      DeleteReferenceEventDTO
      { _drefdtoUuid = event ^. drefUuid
      , _drefdtoKmUuid = event ^. drefKmUuid
      , _drefdtoChapterUuid = event ^. drefChapterUuid
      , _drefdtoQuestionUuid = event ^. drefQuestionUuid
      , _drefdtoReferenceUuid = event ^. drefReferenceUuid
      }

-- -------------------------
-- Follow up question ------
-- -------------------------
instance EventToDTO AddFollowUpQuestionEvent where
  toDTO event =
    AddFollowUpQuestionEventDTO'
      AddFollowUpQuestionEventDTO
      { _afuqdtoUuid = event ^. afuqUuid
      , _afuqdtoKmUuid = event ^. afuqKmUuid
      , _afuqdtoChapterUuid = event ^. afuqChapterUuid
      , _afuqdtoAnswerUuid = event ^. afuqAnswerUuid
      , _afuqdtoQuestionUuid = event ^. afuqQuestionUuid
      , _afuqdtoShortQuestionUuid = event ^. afuqShortQuestionUuid
      , _afuqdtoType = event ^. afuqType
      , _afuqdtoTitle = event ^. afuqTitle
      , _afuqdtoText = event ^. afuqText
      }

instance EventToDTO EditFollowUpQuestionEvent where
  toDTO event =
    EditFollowUpQuestionEventDTO'
      EditFollowUpQuestionEventDTO
      { _efuqdtoUuid = event ^. efuqUuid
      , _efuqdtoKmUuid = event ^. efuqKmUuid
      , _efuqdtoChapterUuid = event ^. efuqChapterUuid
      , _efuqdtoAnswerUuid = event ^. efuqAnswerUuid
      , _efuqdtoQuestionUuid = event ^. efuqQuestionUuid
      , _efuqdtoShortQuestionUuid = event ^. efuqShortQuestionUuid
      , _efuqdtoType = event ^. efuqType
      , _efuqdtoTitle = event ^. efuqTitle
      , _efuqdtoText = event ^. efuqText
      , _efuqdtoAnswerIds = event ^. efuqAnswerIds
      , _efuqdtoExpertIds = event ^. efuqExpertIds
      , _efuqdtoReferenceIds = event ^. efuqReferenceIds
      }

instance EventToDTO DeleteFollowUpQuestionEvent where
  toDTO event =
    DeleteFollowUpQuestionEventDTO'
      DeleteFollowUpQuestionEventDTO
      { _dfuqdtoUuid = event ^. dfuqUuid
      , _dfuqdtoKmUuid = event ^. dfuqKmUuid
      , _dfuqdtoChapterUuid = event ^. dfuqChapterUuid
      , _dfuqdtoAnswerUuid = event ^. dfuqAnswerUuid
      , _dfuqdtoQuestionUuid = event ^. dfuqQuestionUuid
      }
