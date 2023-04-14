module Wizard.Service.UserToken.UserTokenService where

import Data.Foldable (traverse_)
import qualified Data.UUID as U

import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.User.UserTokenDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.User.UserToken
import Wizard.Model.User.UserTokenList
import Wizard.Util.Logger

getTokens :: UserTokenType -> Maybe U.UUID -> AppContextM [UserTokenList]
getTokens tokenType mCurrentTokenUuid = do
  currentUser <- getCurrentUser
  findUserTokensByUserUuidAndType currentUser.uuid tokenType mCurrentTokenUuid

deleteTokenByUuid :: U.UUID -> AppContextM ()
deleteTokenByUuid uuid = do
  _ <- findUserTokenByUuid uuid
  deleteUserTokenByUuid uuid
  return ()

deleteTokenByUserUuid :: U.UUID -> AppContextM ()
deleteTokenByUserUuid userUuid = do
  userTokens <- findUserTokensByUserUuid userUuid
  traverse_ (\t -> deleteUserTokenByUuid t.uuid) userTokens

cleanTokens :: AppContextM ()
cleanTokens = do
  deletedUserTokens <- deleteUserTokensWithExpiration
  logInfoU _CMP_SERVICE $ f' "Deleted the following %s tokens" [show deletedUserTokens]
