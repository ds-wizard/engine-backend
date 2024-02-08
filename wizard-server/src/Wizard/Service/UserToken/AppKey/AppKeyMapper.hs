module Wizard.Service.UserToken.AppKey.AppKeyMapper where

import Data.Time
import qualified Data.UUID as U

import WizardLib.Public.Api.Resource.UserToken.AppKeyCreateDTO
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Service.UserToken.UserTokenMapper

fromAppKeyDTO :: AppKeyCreateDTO -> U.UUID -> U.UUID -> UTCTime -> String -> Maybe String -> U.UUID -> UTCTime -> String -> UserToken
fromAppKeyDTO reqDto uuid userUuid expiresAt secret mUserAgent =
  toUserToken uuid reqDto.name AppKeyUserTokenType userUuid expiresAt secret mUserAgent Nothing
