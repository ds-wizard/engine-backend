module Wizard.Service.UserToken.ApiKey.ApiKeyMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.UserToken.ApiKeyCreateDTO
import Wizard.Service.UserToken.UserTokenMapper
import WizardLib.Public.Model.User.UserToken

fromApiKeyDTO :: ApiKeyCreateDTO -> U.UUID -> U.UUID -> String -> Maybe String -> U.UUID -> UTCTime -> String -> UserToken
fromApiKeyDTO reqDto uuid userUuid secret mUserAgent =
  toUserToken uuid reqDto.name ApiKeyUserTokenType userUuid reqDto.expiresAt secret mUserAgent Nothing
