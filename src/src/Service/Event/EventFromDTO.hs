module Service.Event.EventFromDTO where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.Event.EventDTO
import Common.Types
import KMMigration.Migration.Event.Common
import Model.Event.Answer.AddAnswerEvent
import Model.Event.Answer.DeleteAnswerEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.Chapter.AddChapterEvent
import Model.Event.Chapter.DeleteChapterEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Event
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
class EventFromDTO a where
  fromDTO :: a -> Event

-- -------------------------
-- Knowledge Model ---------
-- -------------------------
instance EventFromDTO AddKnowledgeModelEventDTO where
    fromDTO dto = MkEvent AddKnowledgeModelEvent
      { _akmUuid = dto ^. akmdtoUuid
      , _akmKmUuid = dto ^. akmdtoKmUuid
      , _akmName = dto ^. akmdtoName
      }

instance EventFromDTO EditKnowledgeModelEventDTO where
  fromDTO dto =
    MkEvent
      EditKnowledgeModelEvent
      { _ekmUuid = dto ^. ekmdtoUuid
      , _ekmKmUuid = dto ^. ekmdtoKmUuid
      , _ekmName = dto ^. ekmdtoName
      , _ekmChapterIds = dto ^. ekmdtoChapterIds
      }

-- -------------------------
-- Chapter -----------------
-- -------------------------
instance EventFromDTO AddChapterEventDTO where
  fromDTO dto =
    MkEvent
      AddChapterEvent
      { _achUuid = dto ^. achdtoUuid
      , _achKmUuid = dto ^. achdtoKmUuid
      , _achChapterUuid = dto ^. achdtoChapterUuid
      , _achTitle = dto ^. achdtoTitle
      , _achText = dto ^. achdtoText
      }

instance EventFromDTO EditChapterEventDTO where
  fromDTO dto =
    MkEvent
      EditChapterEvent
      { _echUuid = dto ^. echdtoUuid
      , _echKmUuid = dto ^. echdtoKmUuid
      , _echChapterUuid = dto ^. echdtoChapterUuid
      , _echTitle = dto ^. echdtoTitle
      , _echText = dto ^. echdtoText
      , _echQuestionIds = dto ^. echdtoQuestionIds
      }

instance EventFromDTO DeleteChapterEventDTO where
  fromDTO dto =
    MkEvent
      DeleteChapterEvent
      { _dchUuid = dto ^. dchdtoUuid
      , _dchKmUuid = dto ^. dchdtoKmUuid
      , _dchChapterUuid = dto ^. dchdtoChapterUuid
      }

-- -------------------------
-- Question ----------------
-- -------------------------
instance EventFromDTO AddQuestionEventDTO where
  fromDTO dto =
    MkEvent
      AddQuestionEvent
      { _aqUuid = dto ^. aqdtoUuid
      , _aqKmUuid = dto ^. aqdtoKmUuid
      , _aqChapterUuid = dto ^. aqdtoChapterUuid
      , _aqQuestionUuid = dto ^. aqdtoQuestionUuid
      , _aqType = dto ^. aqdtoType
      , _aqTitle = dto ^. aqdtoTitle
      , _aqText = dto ^. aqdtoText
      }

instance EventFromDTO EditQuestionEventDTO where
  fromDTO dto =
    MkEvent
      EditQuestionEvent
      { _eqUuid = dto ^. eqdtoUuid
      , _eqKmUuid = dto ^. eqdtoKmUuid
      , _eqChapterUuid = dto ^. eqdtoChapterUuid
      , _eqQuestionUuid = dto ^. eqdtoQuestionUuid
      , _eqType = dto ^. eqdtoType
      , _eqTitle = dto ^. eqdtoTitle
      , _eqText = dto ^. eqdtoText
      , _eqAnswerIds = dto ^. eqdtoAnswerIds
      , _eqExpertIds = dto ^. eqdtoExpertIds
      , _eqReferenceIds = dto ^. eqdtoReferenceIds
      }

instance EventFromDTO DeleteQuestionEventDTO where
  fromDTO dto =
    MkEvent
      DeleteQuestionEvent
      { _dqUuid = dto ^. dqdtoUuid
      , _dqKmUuid = dto ^. dqdtoKmUuid
      , _dqChapterUuid = dto ^. dqdtoChapterUuid
      , _dqQuestionUuid = dto ^. dqdtoQuestionUuid
      }

-- -------------------------
-- Answer ------------------
-- -------------------------
instance EventFromDTO AddAnswerEventDTO where
  fromDTO dto =
    MkEvent
      AddAnswerEvent
      { _aansUuid = dto ^. aansdtoUuid
      , _aansKmUuid = dto ^. aansdtoKmUuid
      , _aansChapterUuid = dto ^. aansdtoChapterUuid
      , _aansQuestionUuid = dto ^. aansdtoQuestionUuid
      , _aansAnswerUuid = dto ^. aansdtoAnswerUuid
      , _aansLabel = dto ^. aansdtoLabel
      , _aansAdvice = dto ^. aansdtoAdvice
      }

instance EventFromDTO EditAnswerEventDTO where
  fromDTO dto =
    MkEvent
      EditAnswerEvent
      { _eansUuid = dto ^. eansdtoUuid
      , _eansKmUuid = dto ^. eansdtoKmUuid
      , _eansChapterUuid = dto ^. eansdtoChapterUuid
      , _eansQuestionUuid = dto ^. eansdtoQuestionUuid
      , _eansAnswerUuid = dto ^. eansdtoAnswerUuid
      , _eansLabel = dto ^. eansdtoLabel
      , _eansAdvice = dto ^. eansdtoAdvice
      , _eansFollowingIds = dto ^. eansdtoFollowingIds
      }

instance EventFromDTO DeleteAnswerEventDTO where
  fromDTO dto =
    MkEvent
      DeleteAnswerEvent
      { _dansUuid = dto ^. dansdtoUuid
      , _dansKmUuid = dto ^. dansdtoKmUuid
      , _dansChapterUuid = dto ^. dansdtoChapterUuid
      , _dansQuestionUuid = dto ^. dansdtoQuestionUuid
      , _dansAnswerUuid = dto ^. dansdtoAnswerUuid
      }

-- -------------------------
-- Expert ------------------
-- -------------------------
instance EventFromDTO AddExpertEventDTO where
  fromDTO dto =
    MkEvent
      AddExpertEvent
      { _aexpUuid = dto ^. aexpdtoUuid
      , _aexpKmUuid = dto ^. aexpdtoKmUuid
      , _aexpChapterUuid = dto ^. aexpdtoChapterUuid
      , _aexpQuestionUuid = dto ^. aexpdtoQuestionUuid
      , _aexpExpertUuid = dto ^. aexpdtoExpertUuid
      , _aexpName = dto ^. aexpdtoName
      , _aexpEmail = dto ^. aexpdtoEmail
      }

instance EventFromDTO EditExpertEventDTO where
  fromDTO dto =
    MkEvent
      EditExpertEvent
      { _eexpUuid = dto ^. eexpdtoUuid
      , _eexpKmUuid = dto ^. eexpdtoKmUuid
      , _eexpChapterUuid = dto ^. eexpdtoChapterUuid
      , _eexpQuestionUuid = dto ^. eexpdtoQuestionUuid
      , _eexpExpertUuid = dto ^. eexpdtoExpertUuid
      , _eexpName = dto ^. eexpdtoName
      , _eexpEmail = dto ^. eexpdtoEmail
      }

instance EventFromDTO DeleteExpertEventDTO where
  fromDTO dto =
    MkEvent
      DeleteExpertEvent
      { _dexpUuid = dto ^. dexpdtoUuid
      , _dexpKmUuid = dto ^. dexpdtoKmUuid
      , _dexpChapterUuid = dto ^. dexpdtoChapterUuid
      , _dexpQuestionUuid = dto ^. dexpdtoQuestionUuid
      , _dexpExpertUuid = dto ^. dexpdtoExpertUuid
      }

-- -------------------------
-- Reference ---------------
-- -------------------------
instance EventFromDTO AddReferenceEventDTO where
  fromDTO dto =
    MkEvent
      AddReferenceEvent
      { _arefUuid = dto ^. arefdtoUuid
      , _arefKmUuid = dto ^. arefdtoKmUuid
      , _arefChapterUuid = dto ^. arefdtoChapterUuid
      , _arefQuestionUuid = dto ^. arefdtoQuestionUuid
      , _arefReferenceUuid = dto ^. arefdtoReferenceUuid
      , _arefChapter = dto ^. arefdtoChapter
      }

instance EventFromDTO EditReferenceEventDTO where
  fromDTO dto =
    MkEvent
      EditReferenceEvent
      { _erefUuid = dto ^. erefdtoUuid
      , _erefKmUuid = dto ^. erefdtoKmUuid
      , _erefChapterUuid = dto ^. erefdtoChapterUuid
      , _erefQuestionUuid = dto ^. erefdtoQuestionUuid
      , _erefReferenceUuid = dto ^. erefdtoReferenceUuid
      , _erefChapter = dto ^. erefdtoChapter
      }

instance EventFromDTO DeleteReferenceEventDTO where
  fromDTO dto =
    MkEvent
      DeleteReferenceEvent
      { _drefUuid = dto ^. drefdtoUuid
      , _drefKmUuid = dto ^. drefdtoKmUuid
      , _drefChapterUuid = dto ^. drefdtoChapterUuid
      , _drefQuestionUuid = dto ^. drefdtoQuestionUuid
      , _drefReferenceUuid = dto ^. drefdtoReferenceUuid
      }

-- -------------------------
-- Follow up question ------
-- -------------------------
instance EventFromDTO AddFollowUpQuestionEventDTO where
  fromDTO dto =
    MkEvent
      AddFollowUpQuestionEvent
      { _afuqUuid = dto ^. afuqdtoUuid
      , _afuqKmUuid = dto ^. afuqdtoKmUuid
      , _afuqChapterUuid = dto ^. afuqdtoChapterUuid
      , _afuqAnswerUuid = dto ^. afuqdtoAnswerUuid
      , _afuqQuestionUuid = dto ^. afuqdtoQuestionUuid
      , _afuqType = dto ^. afuqdtoType
      , _afuqTitle = dto ^. afuqdtoTitle
      , _afuqText = dto ^. afuqdtoText
      }

instance EventFromDTO EditFollowUpQuestionEventDTO where
  fromDTO dto =
    MkEvent
      EditFollowUpQuestionEvent
      { _efuqUuid = dto ^. efuqdtoUuid
      , _efuqKmUuid = dto ^. efuqdtoKmUuid
      , _efuqChapterUuid = dto ^. efuqdtoChapterUuid
      , _efuqAnswerUuid = dto ^. efuqdtoAnswerUuid
      , _efuqQuestionUuid = dto ^. efuqdtoQuestionUuid
      , _efuqType = dto ^. efuqdtoType
      , _efuqTitle = dto ^. efuqdtoTitle
      , _efuqText = dto ^. efuqdtoText
      , _efuqAnswerIds = dto ^. efuqdtoAnswerIds
      , _efuqExpertIds = dto ^. efuqdtoExpertIds
      , _efuqReferenceIds = dto ^. efuqdtoReferenceIds
      }

instance EventFromDTO DeleteFollowUpQuestionEventDTO where
  fromDTO dto =
    MkEvent
      DeleteFollowUpQuestionEvent
      { _dfuqUuid = dto ^. dfuqdtoUuid
      , _dfuqKmUuid = dto ^. dfuqdtoKmUuid
      , _dfuqChapterUuid = dto ^. dfuqdtoChapterUuid
      , _dfuqAnswerUuid = dto ^. dfuqdtoAnswerUuid
      , _dfuqQuestionUuid = dto ^. dfuqdtoQuestionUuid
      }
