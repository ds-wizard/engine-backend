module Model.Event.Chapter.EditChapterEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import LensesConfig
import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data EditChapterEvent = EditChapterEvent
  { _echUuid :: UUID
  , _echKmUuid :: UUID
  , _echChapterUuid :: UUID
  , _echTitle :: Maybe String
  , _echText :: Maybe String
  , _echQuestionIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

makeLenses ''EditChapterEvent

instance SameUuid EditChapterEvent Chapter where
  equalsUuid e ch = ch ^. uuid == e ^. echChapterUuid
