module Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent

data KnowledgeModelPackageEvent = KnowledgeModelPackageEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , content :: KnowledgeModelEventData
  , packageId :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
