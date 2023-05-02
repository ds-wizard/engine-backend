module Wizard.Model.Package.PackageList where

import Data.Time
import GHC.Generics

import Wizard.Model.Package.PackageState
import WizardLib.KnowledgeModel.Model.Package.Package (PackagePhase)

data PackageList = PackageList
  { pId :: String
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: PackagePhase
  , description :: String
  , state :: PackageState
  , remoteVersion :: Maybe String
  , remoteOrganizationName :: Maybe String
  , remoteOrganizationLogo :: Maybe String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord PackageList where
  compare a b =
    compare (organizationId a) (organizationId b)
      <> compare (kmId a) (kmId b)
      <> compare (version a) (version b)
