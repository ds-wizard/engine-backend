module Wizard.Model.OpenId.OpenIdClientSession where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data OpenIdClientSession = OpenIdClientSession
  { state :: String
  , nonce :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
