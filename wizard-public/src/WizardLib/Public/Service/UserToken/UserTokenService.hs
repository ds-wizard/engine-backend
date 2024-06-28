module WizardLib.Public.Service.UserToken.UserTokenService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import qualified Data.Cache as C
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import WizardLib.Public.Localization.Messages.Public
import WizardLib.Public.Model.User.UserToken
import WizardLib.Public.Model.User.UserTokenList

getTokens :: AppContextC s sc m => UserTokenType -> Maybe U.UUID -> m [UserTokenList]
getTokens tokenType mCurrentTokenUuid = do
  context <- ask
  let mIdentityUuid = fmap (fromJust . U.fromString) context.identity'
  case mIdentityUuid of
    Just identityUuid -> findUserTokensByUserUuidAndTypeAndTokenUuid identityUuid tokenType mCurrentTokenUuid
    Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER

deleteTokensExceptCurrentSession
  :: ( AppContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     )
  => String
  -> m ()
deleteTokensExceptCurrentSession tokenValue =
  runInTransaction logInfoI logWarnI $ do
    context <- ask
    let mIdentityUuid = fmap (fromJust . U.fromString) context.identity'
    case mIdentityUuid of
      Just identityUuid -> do
        userTokens <- findUserTokensByUserUuidAndType identityUuid "LoginUserTokenType"
        traverse_ deleteUserTokenByUuid . fmap (.uuid) . filter (\ut -> ut.value /= tokenValue) $ userTokens
      Nothing -> throwError $ ForbiddenError _ERROR_SERVICE_USER__MISSING_USER

deleteTokenByUuid
  :: ( AppContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     )
  => U.UUID
  -> m ()
deleteTokenByUuid uuid = do
  _ <- findUserTokenByUuid uuid
  deleteUserTokenByUuid uuid
  return ()

deleteTokensByTenantUuid
  :: ( AppContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     )
  => U.UUID
  -> m ()
deleteTokensByTenantUuid tenantUuid = do
  userTokens <- findUserTokensByTenantUuid tenantUuid
  traverse_ (\t -> deleteUserTokenByUuidAndTenantUuid t.uuid tenantUuid) userTokens

deleteTokenByUserUuid
  :: ( AppContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     )
  => U.UUID
  -> m ()
deleteTokenByUserUuid userUuid = do
  userTokens <- findUserTokensByUserUuid userUuid
  traverse_ (\t -> deleteUserTokenByUuid t.uuid) userTokens

cleanTokens
  :: ( AppContextC s sc m
     , HasField "cache'" s serverCache
     , HasField "userToken" serverCache (C.Cache Int UserToken)
     )
  => m ()
cleanTokens = do
  deletedUserTokens <- deleteUserTokensWithExpiration
  logInfoI _CMP_SERVICE $ f' "Deleted the following %s tokens" [show deletedUserTokens]
