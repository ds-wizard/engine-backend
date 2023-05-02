module WizardLib.KnowledgeModel.Model.Event.Tag.TagEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Common.MapEntry
import WizardLib.KnowledgeModel.Model.Event.EventField

data AddTagEvent = AddTagEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , color :: String
  , annotations :: [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data EditTagEvent = EditTagEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , name :: EventField String
  , description :: EventField (Maybe String)
  , color :: EventField String
  , annotations :: EventField [MapEntry String String]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data DeleteTagEvent = DeleteTagEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
