module Wizard.Api.Resource.Project.Detail.ProjectDetailWsDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.Project.Project

data ProjectDetailWsDTO = ProjectDetailWsDTO
  { name :: String
  , description :: Maybe String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , projectTags :: [String]
  , permissions :: [ProjectPermDTO]
  , documentTemplateId :: Maybe String
  , documentTemplate :: Maybe DocumentTemplateDTO
  , formatUuid :: Maybe U.UUID
  , format :: Maybe DocumentTemplateFormatSimple
  , isTemplate :: Bool
  , labels :: M.Map String [U.UUID]
  , unresolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  , resolvedCommentCounts :: M.Map String (M.Map U.UUID Int)
  }
  deriving (Show, Eq, Generic)
