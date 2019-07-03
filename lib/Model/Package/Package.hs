module Model.Package.Package where

import Data.Time
import GHC.Generics

data Package = Package
  { _packagePId :: String
  , _packageName :: String
  , _packageOrganizationId :: String
  , _packageKmId :: String
  , _packageVersion :: String
  , _packageDescription :: String
  , _packageReadme :: String
  , _packageLicense :: String
  , _packageMetamodelVersion :: Int
  , _packageParentPackageId :: Maybe String
  , _packageCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance Ord Package where
  compare a b =
    (compare (_packageOrganizationId a) (_packageOrganizationId b)) <> (compare (_packageKmId a) (_packageKmId b)) <>
    (compare (_packageVersion a) (_packageVersion b))
