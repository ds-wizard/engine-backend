module Registry.Service.Organization.OrganizationService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time

import LensesConfig
import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Localization.Messages.Internal
import Registry.Localization.Messages.Public
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Context.AppContext
import Registry.Model.Context.AppContextHelpers
import Registry.Model.Organization.Organization
import Registry.Service.ActionKey.ActionKeyService
import Registry.Service.Mail.Mailer
import Registry.Service.Organization.OrganizationMapper
import Registry.Service.Organization.OrganizationValidation
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Crypto (generateRandomString)

getOrganizations :: AppContextM [OrganizationDTO]
getOrganizations = do
  _ <- checkPermissionToListOrganizations
  organizations <- findOrganizations
  return . fmap toDTO $ organizations

createOrganization :: OrganizationCreateDTO -> AppContextM OrganizationDTO
createOrganization reqDto = do
  _ <- validateOrganizationCreateDto reqDto
  token <- generateNewOrgToken
  now <- liftIO getCurrentTime
  let org = fromCreateDTO reqDto UserRole token now now now
  insertOrganization org
  actionKey <- createActionKey (org ^. organizationId) RegistrationActionKey
  _ <-
    sendRegistrationConfirmationMail (toDTO org) (actionKey ^. hash) `catchError`
    (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_ORGANIZATION__ACTIVATION_EMAIL_NOT_SENT)
  sendAnalyticsEmailIfEnabled org
  return . toDTO $ org
  where
    sendAnalyticsEmailIfEnabled org = do
      serverConfig <- asks _appContextApplicationConfig
      when (serverConfig ^. analytics . enabled) $ sendRegistrationCreatedAnalyticsMail (toDTO org)

getOrganizationByOrgId :: String -> AppContextM OrganizationDTO
getOrganizationByOrgId orgId = do
  organization <- findOrganizationByOrgId orgId
  _ <- checkPermissionToOrganization organization
  return . toDTO $ organization

getOrganizationByToken :: String -> AppContextM OrganizationDTO
getOrganizationByToken token = do
  organization <- findOrganizationByToken token
  _ <- checkPermissionToOrganization organization
  return . toDTO $ organization

modifyOrganization :: String -> OrganizationChangeDTO -> AppContextM OrganizationDTO
modifyOrganization orgId reqDto = do
  org <- getOrganizationByOrgId orgId
  _ <- validateOrganizationChangedEmailUniqueness (reqDto ^. email) (org ^. email)
  now <- liftIO getCurrentTime
  let organization = fromChangeDTO reqDto org now
  updateOrganization organization
  return . toDTO $ organization

deleteOrganization :: String -> AppContextM (Maybe AppError)
deleteOrganization orgId = do
  org <- getOrganizationByOrgId orgId
  deleteOrganizationByOrgId orgId
  return Nothing

changeOrganizationTokenByHash :: String -> Maybe String -> AppContextM OrganizationDTO
changeOrganizationTokenByHash orgId maybeHash = do
  akHash <- extractHash maybeHash
  org <- findOrganizationByOrgId orgId
  actionKey <- getActionKeyByHash akHash
  orgToken <- generateNewOrgToken
  now <- liftIO getCurrentTime
  let updatedOrg = (org & token .~ orgToken) & updatedAt .~ now
  updateOrganization updatedOrg
  deleteActionKey (actionKey ^. hash)
  return . toDTO $ updatedOrg

resetOrganizationToken :: ActionKeyDTO -> AppContextM ()
resetOrganizationToken reqDto = do
  org <- findOrganizationByEmail (reqDto ^. email)
  actionKey <- createActionKey (org ^. organizationId) ForgottenTokenActionKey
  _ <-
    sendResetTokenMail (toDTO org) (actionKey ^. hash) `catchError`
    (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_ORGANIZATION__RECOVERY_EMAIL_NOT_SENT)
  return ()

changeOrganizationState :: String -> Maybe String -> OrganizationStateDTO -> AppContextM OrganizationDTO
changeOrganizationState organizationId maybeHash reqDto = do
  akHash <- extractHash maybeHash
  org <- findOrganizationByOrgId organizationId
  actionKey <- getActionKeyByHash akHash
  updatedOrg <- updateOrgTimestamp $ org & active .~ (reqDto ^. active)
  updateOrganization updatedOrg
  deleteActionKey (actionKey ^. hash)
  return . toDTO $ updatedOrg

-- --------------------------------
-- PERMISSIONS
-- --------------------------------
checkPermissionToListOrganizations = do
  currentOrg <- getCurrentOrganization
  if currentOrg ^. role == AdminRole
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "List Organizations"

checkPermissionToOrganization org = do
  currentOrg <- getCurrentOrganization
  if currentOrg ^. role == AdminRole || (org ^. organizationId) == (currentOrg ^. organizationId)
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Detail Organization"

-- --------------------------------
-- PRIVATE
-- --------------------------------
generateNewOrgToken :: AppContextM String
generateNewOrgToken = liftIO $ generateRandomString 256

updateOrgTimestamp :: Organization -> AppContextM Organization
updateOrgTimestamp user = do
  now <- liftIO getCurrentTime
  return $ user & updatedAt .~ now

extractHash maybeHash =
  case maybeHash of
    Just hash -> return hash
    Nothing -> throwError $ UserError _ERROR_SERVICE_ORGANIZATION__REQUIRED_HASH_IN_QUERY_PARAMS
