module Model.Event.KnowledgeModel.EditKnowledgeModelEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import Model.Common
import Model.KnowledgeModel.KnowledgeModel

data EditKnowledgeModelEvent = EditKnowledgeModelEvent
  { _ekmUuid :: UUID
  , _ekmKmUuid :: UUID
  , _ekmName :: Maybe String
  , _ekmChapterIds :: Maybe [UUID]
  } deriving (Show, Eq, Generic)

makeLenses ''EditKnowledgeModelEvent

instance SameUuid EditKnowledgeModelEvent KnowledgeModel where
  equalsUuid e km = km ^. kmUuid == e ^. ekmKmUuid
