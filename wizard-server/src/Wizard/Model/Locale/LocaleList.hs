module Wizard.Model.Locale.LocaleList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data LocaleList = LocaleList
  { uuid :: U.UUID
  , name :: String
  , description :: String
  , code :: String
  , organizationId :: String
  , localeId :: String
  , version :: String
  , defaultLocale :: Bool
  , enabled :: Bool
  , remoteVersion :: Maybe String
  , remoteOrganizationName :: Maybe String
  , remoteOrganizationLogo :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
