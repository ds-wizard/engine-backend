module Wizard.Model.TemporaryFile.TemporaryFile where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data TemporaryFile = TemporaryFile
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  , expiresAt :: UTCTime
  , appUuid :: U.UUID
  , createdBy :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
