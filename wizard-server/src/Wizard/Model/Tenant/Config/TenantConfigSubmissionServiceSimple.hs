module Wizard.Model.Tenant.Config.TenantConfigSubmissionServiceSimple where

import GHC.Generics

data TenantConfigSubmissionServiceSimple = TenantConfigSubmissionServiceSimple
  { sId :: String
  , name :: String
  , description :: String
  }
  deriving (Generic, Eq, Show)
