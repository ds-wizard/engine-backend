module Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageRawEvent where

import Data.Aeson
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelPackageRawEvent = KnowledgeModelPackageRawEvent
  { uuid :: U.UUID
  , parentUuid :: U.UUID
  , entityUuid :: U.UUID
  , content :: Value
  , packageId :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
