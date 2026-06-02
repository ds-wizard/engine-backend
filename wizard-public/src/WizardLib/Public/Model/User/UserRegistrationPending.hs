module WizardLib.Public.Model.User.UserRegistrationPending where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserRegistrationPending serviceType = UserRegistrationPending
  { uuid :: U.UUID
  , hash :: String
  , serviceType :: serviceType
  , providerUuid :: U.UUID
  , externalId :: String
  , externalLabel :: Maybe String
  , email :: Maybe String
  , firstName :: Maybe String
  , lastName :: Maybe String
  , imageUrl :: Maybe String
  , affiliation :: Maybe String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq serviceType => Eq (UserRegistrationPending serviceType) where
  a == b =
    a.uuid == b.uuid
      && a.hash == b.hash
      && a.serviceType == b.serviceType
      && a.providerUuid == b.providerUuid
      && a.externalId == b.externalId
      && a.externalLabel == b.externalLabel
      && a.email == b.email
      && a.firstName == b.firstName
      && a.lastName == b.lastName
      && a.imageUrl == b.imageUrl
      && a.affiliation == b.affiliation
      && a.tenantUuid == b.tenantUuid
      && a.createdAt == b.createdAt
