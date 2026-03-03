module Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

data KnowledgeModelEditorList = KnowledgeModelEditorList
  { uuid :: U.UUID
  , name :: String
  , kmId :: String
  , version :: String
  , state :: KnowledgeModelEditorState
  , previousPackageUuid :: Maybe U.UUID
  , forkOfPackage :: Maybe KnowledgeModelPackageSuggestion
  , createdBy :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq KnowledgeModelEditorList where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.kmId == b.kmId
      && a.version == b.version
      && a.state == b.state
      && a.previousPackageUuid == b.previousPackageUuid
      && a.forkOfPackage == b.forkOfPackage
      && a.createdBy == b.createdBy
