module Wizard.Model.KnowledgeModel.KnowledgeModelCache where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data KnowledgeModelCache = KnowledgeModelCache
  { knowledgeModelPackageUuid :: U.UUID
  , tagUuids :: [U.UUID]
  , knowledgeModel :: KnowledgeModel
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
