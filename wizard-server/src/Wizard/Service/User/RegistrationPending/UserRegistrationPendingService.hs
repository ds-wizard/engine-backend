module Wizard.Service.User.RegistrationPending.UserRegistrationPendingService (
  completeExternalRegistration,
  cleanUserRegistrationPending,
) where

import Control.Monad (void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Char (toLower)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Mapping.User.UserRegistrationPendingServiceType ()
import Wizard.Localization.Messages.Internal
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Model.User.UserRegistrationPendingServiceType
import Wizard.Model.UserEmailLink.UserEmailLinkType
import Wizard.Service.Mail.Mailer
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import Wizard.Service.Tenant.TenantHelper
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import Wizard.Service.User.UserValidation
import Wizard.Service.UserEmailLink.UserEmailLinkService
import Wizard.Service.UserToken.Login.LoginService
import Wizard.Service.UserToken.Login.LoginValidation
import WizardLib.Public.Api.Resource.User.UserFromExternalDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Database.DAO.User.UserOpenIdIdentityDAO
import WizardLib.Public.Database.DAO.User.UserRegistrationPendingDAO
import WizardLib.Public.Model.User.Role
import WizardLib.Public.Model.User.UserRegistrationPending
import qualified WizardLib.Public.Service.User.UserOpenIdIdentityMapper as UserOpenIdIdentityMapper
import WizardLib.Public.Service.User.UserRegistrationPendingService (cleanUserRegistrationPending)

completeExternalRegistration :: UserFromExternalDTO -> Maybe String -> Maybe String -> AppContextM UserTokenDTO
completeExternalRegistration reqDto _mAcceptLanguages mUserAgent =
  runInTransaction $ do
    (pending :: UserRegistrationPending UserRegistrationPendingServiceType) <-
      findUserRegistrationPendingByHash reqDto.hash
    now <- liftIO getCurrentTime
    tenantUuid <- asks currentTenantUuid
    case pending.email of
      Just idpEmail ->
        when (fmap toLower idpEmail /= fmap toLower reqDto.email) $
          throwError . UserError $
            _ERROR_VALIDATION__USER_EMAIL_FROM_IDP_CANNOT_BE_CHANGED
      Nothing -> return ()
    validateUserEmailUniqueness reqDto.email tenantUuid
    let emailVerified = case pending.email of
          Just _ -> True
          Nothing -> False
    user <- createUserForPending pending reqDto emailVerified now
    identityUuid <- liftIO generateUuid
    let identity = UserOpenIdIdentityMapper.fromPending identityUuid pending (user :: User).uuid now
    void $ insertUserOpenIdIdentity identity
    deleteUserRegistrationPendingByHash reqDto.hash
    if emailVerified
      then do
        tcAuthentication <- getCurrentTenantConfigAuthentication
        validateLoginEnabled tcAuthentication user
        createLoginToken user mUserAgent Nothing
      else do
        userEmailLink <- createUserEmailLink (user :: User).uuid RegistrationUserEmailLinkType tenantUuid
        clientUrl <- getClientUrl
        catchError
          (sendRegistrationConfirmationMail user userEmailLink.hash clientUrl)
          (\_ -> throwError $ GeneralServerError _ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT)
        return EmailVerificationRequiredDTO

-- --------------------------------
-- PRIVATE
-- --------------------------------
createUserForPending
  :: UserRegistrationPending UserRegistrationPendingServiceType
  -> UserFromExternalDTO
  -> Bool
  -> UTCTime
  -> AppContextM User
createUserForPending pending reqDto emailVerified now = do
  checkUserLimit
  checkActiveUserLimit
  tenantUuid <- asks currentTenantUuid
  uUuid <- liftIO generateUuid
  password <- liftIO . fmap U.toString $ generateUuid
  uPasswordHash <- generatePasswordHash password
  tcAuthentication <- getCurrentTenantConfigAuthentication
  let role = tcAuthentication.defaultRoleUuid
  uRole <- getRoleForUser role
  let user =
        fromUserExternalDTO
          uUuid
          reqDto.firstName
          reqDto.lastName
          (toLower <$> reqDto.email)
          uPasswordHash
          role
          uRole.permissions
          uRole.name
          emailVerified
          pending.imageUrl
          tenantUuid
          now
  insertUser user
  return user
