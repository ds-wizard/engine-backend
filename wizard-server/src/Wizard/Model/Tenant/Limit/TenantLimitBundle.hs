module Wizard.Model.Tenant.Limit.TenantLimitBundle where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data TenantLimitBundle = TenantLimitBundle
  { uuid :: U.UUID
  , users :: Int
  , activeUsers :: Int
  , knowledgeModels :: Int
  , knowledgeModelEditors :: Int
  , documentTemplates :: Int
  , documentTemplateDrafts :: Int
  , questionnaires :: Int
  , documents :: Int
  , locales :: Int
  , storage :: Int64
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
