module Wizard.Service.UserToken.ApiKey.ApiKeyMapper where

import Data.Time
import qualified Data.UUID as U

import WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateDTO
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Service.UserToken.UserTokenMapper

fromApiKeyDTO :: ApiKeyCreateDTO -> U.UUID -> U.UUID -> String -> Maybe String -> U.UUID -> UTCTime -> String -> UserToken
fromApiKeyDTO reqDto uuid userUuid secret mUserAgent =
  toUserToken uuid reqDto.name ApiKeyUserTokenType userUuid reqDto.expiresAt secret mUserAgent Nothing
