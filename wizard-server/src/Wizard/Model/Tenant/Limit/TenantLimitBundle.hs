module Wizard.Model.Tenant.Limit.TenantLimitBundle where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data TenantLimitBundle = TenantLimitBundle
  { uuid :: U.UUID
  , users :: Maybe Int
  , activeUsers :: Maybe Int
  , knowledgeModels :: Maybe Int
  , branches :: Maybe Int
  , documentTemplates :: Maybe Int
  , documentTemplateDrafts :: Maybe Int
  , questionnaires :: Maybe Int
  , documents :: Maybe Int
  , locales :: Maybe Int
  , storage :: Maybe Int64
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
