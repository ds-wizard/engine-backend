module Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageList where

import Data.Time
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage (KnowledgeModelPackagePhase)

data KnowledgeModelPackageList = KnowledgeModelPackageList
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , description :: String
  , nonEditable :: Bool
  , remoteVersion :: Maybe String
  , remoteOrganizationName :: Maybe String
  , remoteOrganizationLogo :: Maybe String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord KnowledgeModelPackageList where
  compare a b =
    compare (organizationId a) (organizationId b)
      <> compare (kmId a) (kmId b)
      <> compare (version a) (version b)
