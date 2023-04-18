module Wizard.Service.UserToken.ApiKey.ApiKeyMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.UserToken.ApiKeyCreateDTO
import Wizard.Model.User.UserToken
import Wizard.Service.UserToken.UserTokenMapper

fromApiKeyDTO :: ApiKeyCreateDTO -> U.UUID -> U.UUID -> String -> Maybe String -> U.UUID -> UTCTime -> UserToken
fromApiKeyDTO reqDto uuid userUuid secret mUserAgent =
  toUserToken uuid reqDto.name ApiKeyUserTokenType userUuid reqDto.expiresAt secret mUserAgent Nothing
