module Wizard.Service.UserEmailLink.UserEmailLinkService where

import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Wizard.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Wizard.Model.Context.AppContext (AppContextM)
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.UserEmailLink.UserEmailLinkType
import Wizard.Service.Tenant.Config.ConfigService

createUserEmailLink :: U.UUID -> UserEmailLinkType -> U.UUID -> AppContextM (UserEmailLink U.UUID UserEmailLinkType)
createUserEmailLink userUuid actionType tenantUuid = do
  hash <- liftIO generateUuid
  createUserEmailLinkWithHash userUuid actionType tenantUuid (U.toString hash)

createUserEmailLinkWithHash :: U.UUID -> UserEmailLinkType -> U.UUID -> String -> AppContextM (UserEmailLink U.UUID UserEmailLinkType)
createUserEmailLinkWithHash userUuid actionType tenantUuid hash =
  runInTransaction $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let userEmailLink =
          UserEmailLink
            { uuid = uuid
            , identity = userUuid
            , aType = actionType
            , hash = hash
            , tenantUuid = tenantUuid
            , createdAt = now
            }
    insertUserEmailLink userEmailLink
    return userEmailLink

cleanUserEmailLinks :: AppContextM ()
cleanUserEmailLinks = void deleteUserEmailLinksExpiredByTenantConfig

validateUserEmailLinkNotExpired :: UserEmailLink identity aType -> AppContextM ()
validateUserEmailLinkNotExpired userEmailLink = do
  tcAuthentication <- getTenantConfigAuthenticationByUuid userEmailLink.tenantUuid
  now <- liftIO getCurrentTime
  let timeDelta = realToFrac . toInteger $ tcAuthentication.internal.userEmailLinkExpiration * 3600
  when (addUTCTime timeDelta userEmailLink.createdAt < now) (throwError $ UserError _ERROR_SERVICE_USER_EMAIL_LINK__EXPIRED)
