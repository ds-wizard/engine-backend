module Registry.Service.Organization.OrganizationService where

import Control.Monad (when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Time

import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Database.DAO.ActionKey.ActionKeyDAO
import Registry.Database.DAO.Common
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Localization.Messages.Internal
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.AppContextHelpers
import Registry.Model.Organization.Organization
import Registry.Service.ActionKey.ActionKeyService
import Registry.Service.Mail.Mailer
import Registry.Service.Organization.OrganizationMapper
import Registry.Service.Organization.OrganizationValidation
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Localization.Messages.Public
import Shared.Model.Config.ServerConfig
import Shared.Model.Error.Error
import Shared.Util.Crypto (generateRandomString)

getOrganizations :: AppContextM [OrganizationDTO]
getOrganizations = do
  _ <- checkPermissionToListOrganizations
  organizations <- findOrganizations
  return . fmap toDTO $ organizations

getSimpleOrganizations :: AppContextM [OrganizationSimpleDTO]
getSimpleOrganizations = do
  organizations <- findOrganizations
  return . fmap toSimpleDTO $ organizations

createOrganization :: OrganizationCreateDTO -> Maybe String -> AppContextM OrganizationDTO
createOrganization reqDto mCallbackUrl =
  runInTransaction $ do
    _ <- validateOrganizationCreateDto reqDto
    token <- generateNewOrgToken
    now <- liftIO getCurrentTime
    let org = fromCreateDTO reqDto UserRole token now now now
    insertOrganization org
    actionKey <- createActionKey org.organizationId RegistrationActionKey
    _ <-
      sendRegistrationConfirmationMail (toDTO org) actionKey.hash mCallbackUrl
        `catchError` (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_ORGANIZATION__ACTIVATION_EMAIL_NOT_SENT)
    sendAnalyticsEmailIfEnabled org
    return . toDTO $ org
  where
    sendAnalyticsEmailIfEnabled org = do
      serverConfig <- asks serverConfig
      when serverConfig.analytics.enabled $ sendRegistrationCreatedAnalyticsMail (toDTO org)

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
modifyOrganization orgId reqDto =
  runInTransaction $ do
    org <- getOrganizationByOrgId orgId
    _ <- validateOrganizationChangedEmailUniqueness reqDto.email org.email
    now <- liftIO getCurrentTime
    let organization = fromChangeDTO reqDto org now
    updateOrganization organization
    return . toDTO $ organization

deleteOrganization :: String -> AppContextM (Maybe AppError)
deleteOrganization orgId =
  runInTransaction $ do
    org <- getOrganizationByOrgId orgId
    deleteOrganizationByOrgId orgId
    return Nothing

changeOrganizationTokenByHash :: String -> String -> AppContextM OrganizationDTO
changeOrganizationTokenByHash orgId hash =
  runInTransaction $ do
    actionKey <- findActionKeyByHash hash
    org <- findOrganizationByOrgId actionKey.organizationId
    orgToken <- generateNewOrgToken
    now <- liftIO getCurrentTime
    let updatedOrg = org {token = orgToken, updatedAt = now} :: Organization
    updateOrganization updatedOrg
    deleteActionKeyByHash actionKey.hash
    return . toDTO $ updatedOrg

resetOrganizationToken :: ActionKeyDTO -> AppContextM ()
resetOrganizationToken reqDto =
  runInTransaction $ do
    validateOrganizationEmailExistence reqDto.email
    org <- findOrganizationByEmail reqDto.email
    actionKey <- createActionKey org.organizationId ForgottenTokenActionKey
    _ <-
      sendResetTokenMail (toDTO org) actionKey.hash
        `catchError` (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_ORGANIZATION__RECOVERY_EMAIL_NOT_SENT)
    return ()

changeOrganizationState :: String -> String -> OrganizationStateDTO -> AppContextM OrganizationDTO
changeOrganizationState orgId hash reqDto =
  runInTransaction $ do
    actionKey <- findActionKeyByHash hash
    org <- findOrganizationByOrgId actionKey.organizationId
    updatedOrg <- updateOrgTimestamp $ org {active = reqDto.active}
    updateOrganization updatedOrg
    deleteActionKeyByHash actionKey.hash
    return . toDTO $ updatedOrg

-- --------------------------------
-- PERMISSIONS
-- --------------------------------
checkPermissionToListOrganizations = do
  currentOrg <- getCurrentOrganization
  if currentOrg.oRole == AdminRole
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "List Organizations"

checkPermissionToOrganization org = do
  currentOrg <- getCurrentOrganization
  if currentOrg.oRole == AdminRole || org.organizationId == currentOrg.organizationId
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Detail Organization"

-- --------------------------------
-- PRIVATE
-- --------------------------------
generateNewOrgToken :: AppContextM String
generateNewOrgToken = liftIO $ generateRandomString 256

updateOrgTimestamp :: Organization -> AppContextM Organization
updateOrgTimestamp org = do
  now <- liftIO getCurrentTime
  return $ org {updatedAt = now}
