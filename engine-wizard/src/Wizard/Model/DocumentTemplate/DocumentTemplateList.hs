module Wizard.Model.DocumentTemplate.DocumentTemplateList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Package.PackagePattern
import Wizard.Model.DocumentTemplate.DocumentTemplateState

data DocumentTemplateList = DocumentTemplateList
  { tId :: String
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , allowedPackages :: [PackagePattern]
  , formats :: [DocumentTemplateFormat]
  , state :: DocumentTemplateState
  , remoteVersion :: Maybe String
  , remoteOrganizationName :: Maybe String
  , remoteOrganizationLogo :: Maybe String
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplateList where
  compare a b =
    compare a.organizationId b.organizationId
      <> compare a.templateId b.templateId
      <> compare a.version b.version
