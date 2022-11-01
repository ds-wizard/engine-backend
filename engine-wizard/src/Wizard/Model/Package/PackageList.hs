module Wizard.Model.Package.PackageList where

import Data.Time
import GHC.Generics

import Wizard.Model.Package.PackageState

data PackageList =
  PackageList
    { _packageListPId :: String
    , _packageListName :: String
    , _packageListOrganizationId :: String
    , _packageListKmId :: String
    , _packageListVersion :: String
    , _packageListDescription :: String
    , _packageListState :: PackageState
    , _packageListRemoteVersion :: Maybe String
    , _packageListRemoteOrganizationName :: Maybe String
    , _packageListRemoteOrganizationLogo :: Maybe String
    , _packageListCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

instance Ord PackageList where
  compare a b =
    compare (_packageListOrganizationId a) (_packageListOrganizationId b) <>
    compare (_packageListKmId a) (_packageListKmId b) <> compare (_packageListVersion a) (_packageListVersion b)
