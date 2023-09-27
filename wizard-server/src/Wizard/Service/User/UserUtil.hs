module Wizard.Service.User.UserUtil where

import Control.Monad.Reader (asks)
import qualified Crypto.PasswordStore as PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe (isJust, isNothing)
import qualified Data.UUID as U

import Shared.Common.Model.Common.SensitiveData
import Shared.Common.Util.String (splitOn)
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Model.User.UserEM ()
import Wizard.Service.Tenant.Config.ConfigService

getDecryptedUser :: U.UUID -> AppContextM User
getDecryptedUser userUuid = do
  serverConfig <- asks serverConfig
  user <- findUserByUuid userUuid
  return $ process serverConfig.general.secret user

verifyPassword :: String -> String -> Bool
verifyPassword incomingPassword passwordHashFromDB =
  case splitOn ":" passwordHashFromDB of
    ["pbkdf1", hashFromDB] -> PasswordStore.verifyPassword (BS.pack incomingPassword) (BS.pack hashFromDB)
    ["pbkdf2", hashFromDB] ->
      PasswordStore.verifyPasswordWith PasswordStore.pbkdf2 (2 ^) (BS.pack incomingPassword) (BS.pack hashFromDB)
    _ -> False

isConsentRequired :: Maybe User -> AppContextM Bool
isConsentRequired mUserFromDb = do
  tenantConfig <- getCurrentTenantConfig
  return $ isNothing mUserFromDb && (isJust tenantConfig.privacyAndSupport.privacyUrl || isJust tenantConfig.privacyAndSupport.termsOfServiceUrl)
