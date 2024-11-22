module WizardLib.Public.Model.Tenant.Limit.TenantLimitBundleChange where

import Data.Aeson
import GHC.Generics
import GHC.Int

import Shared.Common.Util.Aeson

data TenantLimitBundleChange = TenantLimitBundleChange
  { users :: Int
  , activeUsers :: Int
  , knowledgeModels :: Int
  , branches :: Int
  , documentTemplates :: Int
  , documentTemplateDrafts :: Int
  , questionnaires :: Int
  , documents :: Int
  , locales :: Int
  , storage :: Int64
  }
  deriving (Show, Eq, Generic)

instance FromJSON TenantLimitBundleChange where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantLimitBundleChange where
  toJSON = genericToJSON jsonOptions
