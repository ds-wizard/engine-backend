module Wizard.Model.KnowledgeModel.KnowledgeModelSecret where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelSecret = KnowledgeModelSecret
  { uuid :: U.UUID
  , name :: String
  , value :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq KnowledgeModelSecret where
  a == b =
    uuid a == uuid b
      && name a == name b
      && value a == value b
      && tenantUuid a == tenantUuid b
