module WizardLib.Public.Model.TemporaryFile.TemporaryFile where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data TemporaryFile = TemporaryFile
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , expiresAt :: UTCTime
  , tenantUuid :: U.UUID
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
