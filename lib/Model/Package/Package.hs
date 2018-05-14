module Model.Package.Package where

import GHC.Generics

import Model.Event.Event

data Package = Package
  { _packagePId :: String
  , _packageName :: String
  , _packageOrganizationId :: String
  , _packageKmId :: String
  , _packageVersion :: String
  , _packageDescription :: String
  , _packageParentPackageId :: Maybe String
  } deriving (Show, Eq, Generic)

data PackageWithEvents = PackageWithEvents
  { _packageWithEventsPId :: String
  , _packageWithEventsName :: String
  , _packageWithEventsOrganizationId :: String
  , _packageWithEventsKmId :: String
  , _packageWithEventsVersion :: String
  , _packageWithEventsDescription :: String
  , _packageWithEventsParentPackageId :: Maybe String
  , _packageWithEventsEvents :: [Event]
  } deriving (Show, Eq, Generic)
