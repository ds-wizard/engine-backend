module Wizard.Service.User.UserUtil where

import qualified Crypto.PasswordStore as PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe (isJust, isNothing)

import Shared.Common.Util.String (splitOn)
import Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Model.User.UserSubmissionPropEM ()

verifyPassword :: String -> String -> Bool
verifyPassword incomingPassword passwordHashFromDB =
  case splitOn ":" passwordHashFromDB of
    ["pbkdf1", hashFromDB] -> PasswordStore.verifyPassword (BS.pack incomingPassword) (BS.pack hashFromDB)
    ["pbkdf2", hashFromDB] ->
      PasswordStore.verifyPasswordWith PasswordStore.pbkdf2 (2 ^) (BS.pack incomingPassword) (BS.pack hashFromDB)
    _ -> False

isConsentRequired :: Maybe User -> AppContextM Bool
isConsentRequired mUserFromDb = do
  tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
  return $ isNothing mUserFromDb && (isJust tcPrivacyAndSupport.privacyUrl || isJust tcPrivacyAndSupport.termsOfServiceUrl)
