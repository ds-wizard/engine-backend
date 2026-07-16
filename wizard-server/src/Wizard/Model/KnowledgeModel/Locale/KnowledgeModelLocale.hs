module Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelLocale = KnowledgeModelLocale
  { uuid :: U.UUID
  , name :: String
  , code :: String
  , knowledgeModelPackageUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
