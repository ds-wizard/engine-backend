module Shared.Audit.Model.Audit.Audit where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Audit = Audit
  { uuid :: U.UUID
  , component :: String
  , action :: String
  , entity :: String
  , body :: M.Map String String
  , createdBy :: Maybe U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
