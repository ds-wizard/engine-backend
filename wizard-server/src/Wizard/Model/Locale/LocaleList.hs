module Wizard.Model.Locale.LocaleList where

import Data.Time
import GHC.Generics

import Wizard.Model.Locale.LocaleState

data LocaleList = LocaleList
  { lId :: String
  , name :: String
  , description :: String
  , code :: String
  , organizationId :: String
  , localeId :: String
  , version :: String
  , defaultLocale :: Bool
  , enabled :: Bool
  , state :: LocaleState
  , remoteVersion :: Maybe String
  , remoteOrganizationName :: Maybe String
  , remoteOrganizationLogo :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
