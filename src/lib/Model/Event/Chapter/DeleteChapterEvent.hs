module Model.Event.Chapter.DeleteChapterEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data DeleteChapterEvent = DeleteChapterEvent
  { _dchUuid :: UUID
  , _dchKmUuid :: UUID
  , _dchChapterUuid :: UUID
  } deriving (Show, Eq, Generic)

makeLenses ''DeleteChapterEvent

instance SameUuid DeleteChapterEvent KnowledgeModel where
  equalsUuid e km = km ^. kmUuid == e ^. dchKmUuid

instance SameUuid DeleteChapterEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. dchChapterUuid
