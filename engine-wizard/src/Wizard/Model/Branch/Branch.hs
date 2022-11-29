module Wizard.Model.Branch.Branch where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Branch = Branch
  { uuid :: U.UUID
  , name :: String
  , kmId :: String
  , previousPackageId :: Maybe String
  , createdBy :: Maybe U.UUID
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
