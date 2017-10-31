module Model.Event.Expert.AddExpertEvent where

import Control.Lens
import Data.UUID
import GHC.Generics

import KMMigration.Migration.Event.Common
import KMMigration.Model.Common
import Model.KnowledgeModel.KnowledgeModel

data AddExpertEvent = AddExpertEvent
  { _aexpUuid :: UUID
  , _aexpKmUuid :: UUID
  , _aexpChapterUuid :: UUID
  , _aexpQuestionUuid :: UUID
  , _aexpExpertUuid :: UUID
  , _aexpName :: String
  , _aexpEmail :: String
  } deriving (Show, Eq, Generic)

makeLenses ''AddExpertEvent

instance SameUuid AddExpertEvent Chapter where
  equalsUuid e ch = ch ^. chUuid == e ^. aexpChapterUuid

instance SameUuid AddExpertEvent Question where
  equalsUuid e q = q ^. qUuid == e ^. aexpQuestionUuid

instance SameUuid AddExpertEvent Expert where
  equalsUuid e exp = exp ^. expUuid == e ^. aexpExpertUuid
