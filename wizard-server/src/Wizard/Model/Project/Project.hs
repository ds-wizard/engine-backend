module Wizard.Model.Project.Project where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Project.Acl.ProjectPerm

data ProjectVisibility
  = PrivateProjectVisibility
  | VisibleViewProjectVisibility
  | VisibleCommentProjectVisibility
  | VisibleEditProjectVisibility
  deriving (Show, Eq, Generic, Read)

data ProjectSharing
  = RestrictedProjectSharing
  | AnyoneWithLinkViewProjectSharing
  | AnyoneWithLinkCommentProjectSharing
  | AnyoneWithLinkEditProjectSharing
  deriving (Show, Eq, Generic, Read)

data Project = Project
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackageUuid :: U.UUID
  , selectedQuestionTagUuids :: [U.UUID]
  , projectTags :: [String]
  , documentTemplateUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  , creatorUuid :: Maybe U.UUID
  , permissions :: [ProjectPerm]
  , isTemplate :: Bool
  , squashed :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq Project where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.visibility == b.visibility
      && a.sharing == b.sharing
      && a.knowledgeModelPackageUuid == b.knowledgeModelPackageUuid
      && a.selectedQuestionTagUuids == b.selectedQuestionTagUuids
      && a.projectTags == b.projectTags
      && a.documentTemplateUuid == b.documentTemplateUuid
      && a.formatUuid == b.formatUuid
      && a.creatorUuid == b.creatorUuid
      && a.permissions == b.permissions
      && a.isTemplate == b.isTemplate
      && a.squashed == b.squashed
      && a.tenantUuid == b.tenantUuid
