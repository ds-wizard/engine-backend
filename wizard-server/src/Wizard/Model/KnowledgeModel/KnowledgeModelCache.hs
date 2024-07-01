module Wizard.Model.KnowledgeModel.KnowledgeModelCache where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data KnowledgeModelCache = KnowledgeModelCache
  { packageId :: String
  , tagUuids :: [U.UUID]
  , knowledgeModel :: KnowledgeModel
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
