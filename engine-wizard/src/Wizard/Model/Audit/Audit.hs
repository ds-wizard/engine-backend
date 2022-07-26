module Wizard.Model.Audit.Audit where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Audit =
  Audit
    { _auditUuid :: U.UUID
    , _auditComponent :: String
    , _auditAction :: String
    , _auditEntity :: String
    , _auditBody :: M.Map String String
    , _auditCreatedBy :: Maybe U.UUID
    , _auditAppUuid :: U.UUID
    , _auditCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
