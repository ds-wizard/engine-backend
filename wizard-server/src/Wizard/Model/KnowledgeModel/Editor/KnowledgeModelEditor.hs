module Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelEditor = KnowledgeModelEditor
  { uuid :: U.UUID
  , name :: String
  , kmId :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  , previousPackageId :: Maybe String
  , metamodelVersion :: Int
  , squashed :: Bool
  , createdBy :: Maybe U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
