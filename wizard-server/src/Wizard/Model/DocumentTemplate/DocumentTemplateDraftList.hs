module Wizard.Model.DocumentTemplate.DocumentTemplateDraftList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateDraftList = DocumentTemplateDraftList
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , description :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplateDraftList where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.templateId b.templateId
      <> compare a.version b.version
