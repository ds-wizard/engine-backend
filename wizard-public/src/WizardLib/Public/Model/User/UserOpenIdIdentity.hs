module WizardLib.Public.Model.User.UserOpenIdIdentity where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserOpenIdIdentity = UserOpenIdIdentity
  { uuid :: U.UUID
  , externalId :: String
  , externalLabel :: Maybe String
  , userUuid :: U.UUID
  , providerUuid :: U.UUID
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
