module KMMigration.Migration.Event.Question.EditQuestionEvent where

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

import Control.Lens

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import KMMigration.Model.KnowledgeModel

data EditQuestionEvent = EditQuestionEvent
  { _eqUuid :: UUID
  , _eqKmUuid :: UUID
  , _eqChapterUuid :: UUID
  , _eqQuestionUuid :: UUID
  , _eqType :: Maybe String
  , _eqTitle :: Maybe String
  , _eqText :: Maybe String
  , _eqAnswerIds :: Maybe [UUID]
  , _eqExpertIds :: Maybe [UUID]
  , _eqReferenceIds :: Maybe [UUID]
  }

makeLenses ''EditQuestionEvent

instance SameUuid EditQuestionEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. eqChapterUuid

instance SameUuid EditQuestionEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. eqQuestionUuid
