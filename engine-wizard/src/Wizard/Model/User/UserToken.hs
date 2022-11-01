module Wizard.Model.User.UserToken where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserToken =
  UserToken
    { _userTokenUuid :: U.UUID
    , _userTokenUserUuid :: U.UUID
    , _userTokenValue :: String
    , _userTokenSessionState :: Maybe String
    , _userTokenAppUuid :: U.UUID
    , _userTokenCreatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq UserToken where
  a == b =
    _userTokenUuid a == _userTokenUuid b &&
    _userTokenUserUuid a == _userTokenUserUuid b &&
    _userTokenValue a == _userTokenValue b &&
    _userTokenSessionState a == _userTokenSessionState b && _userTokenAppUuid a == _userTokenAppUuid b
