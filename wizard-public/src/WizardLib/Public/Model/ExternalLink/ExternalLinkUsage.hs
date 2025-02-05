module WizardLib.Public.Model.ExternalLink.ExternalLinkUsage where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data ExternalLinkUsage = ExternalLinkUsage
  { uuid :: U.UUID
  , url :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
