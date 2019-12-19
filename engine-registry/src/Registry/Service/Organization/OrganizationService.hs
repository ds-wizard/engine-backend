module Registry.Service.Organization.OrganizationService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Time

import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.LensesConfig
import Registry.Localization
import Registry.Localization.Messages.Public
import Registry.Model.ActionKey.ActionKey
import Registry.Model.Context.AppContext
import Registry.Model.Context.AppContextHelpers
import Registry.Model.Organization.Organization
import Registry.Service.ActionKey.ActionKeyService
import Registry.Service.Mail.Mailer
import Registry.Service.Organization.OrganizationMapper
import Registry.Service.Organization.OrganizationValidation
import Registry.Util.StringGenerator (generateRandomString)
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper, createHemHelper)

getOrganizations :: AppContextM (Either AppError [OrganizationDTO])
getOrganizations =
  checkPermissionToListOrganizations $ heFindOrganizations $ \organizations ->
    return . Right . fmap toDTO $ organizations

createOrganization :: OrganizationCreateDTO -> AppContextM (Either AppError OrganizationDTO)
createOrganization reqDto =
  heValidateOrganizationCreateDto reqDto $ do
    token <- generateNewOrgToken
    now <- liftIO getCurrentTime
    let org = fromCreateDTO reqDto UserRole token now now now
    insertOrganization org
    heCreateActionKey (org ^. organizationId) RegistrationActionKey $ \actionKey -> do
      emailResult <- sendRegistrationConfirmationMail (toDTO org) (actionKey ^. hash)
      case emailResult of
        Left errMessage -> return . Left $ GeneralServerError _ERROR_SERVICE_ORGANIZATION__ACTIVATION_EMAIL_NOT_SENT
        _ -> do
          sendAnalyticsEmailIfEnabled org
          return . Right $ toDTO org
  where
    sendAnalyticsEmailIfEnabled org = do
      appConfig <- asks _appContextApplicationConfig
      if appConfig ^. analytics . enabled
        then sendRegistrationCreatedAnalyticsMail (toDTO org)
        else return $ Right ()

getOrganizationByOrgId :: String -> AppContextM (Either AppError OrganizationDTO)
getOrganizationByOrgId orgId =
  heFindOrganizationByOrgId orgId $ \organization ->
    checkPermissionToOrganization organization $ return . Right . toDTO $ organization

getOrganizationByToken :: String -> AppContextM (Either AppError OrganizationDTO)
getOrganizationByToken token =
  heFindOrganizationByToken token $ \organization ->
    checkPermissionToOrganization organization $ return . Right . toDTO $ organization

modifyOrganization :: String -> OrganizationChangeDTO -> AppContextM (Either AppError OrganizationDTO)
modifyOrganization orgId reqDto =
  heGetOrganizationByOrgId orgId $ \org ->
    heValidateOrganizationChangedEmailUniqueness (reqDto ^. email) (org ^. email) $ do
      now <- liftIO getCurrentTime
      let organization = fromChangeDTO reqDto org now
      updateOrganization organization
      return . Right . toDTO $ organization

deleteOrganization :: String -> AppContextM (Maybe AppError)
deleteOrganization orgId =
  hmGetOrganizationByOrgId orgId $ \org -> do
    deleteOrganizationByOrgId orgId
    return Nothing

changeOrganizationTokenByHash :: String -> Maybe String -> AppContextM (Either AppError OrganizationDTO)
changeOrganizationTokenByHash orgId maybeHash =
  heExtractHash maybeHash $ \akHash ->
    heFindOrganizationByOrgId orgId $ \org ->
      heGetActionKeyByHash akHash $ \actionKey -> do
        orgToken <- generateNewOrgToken
        now <- liftIO getCurrentTime
        let updatedOrg = (org & token .~ orgToken) & updatedAt .~ now
        updateOrganization updatedOrg
        deleteActionKey (actionKey ^. hash)
        return . Right . toDTO $ updatedOrg

resetOrganizationToken :: ActionKeyDTO -> AppContextM (Maybe AppError)
resetOrganizationToken reqDto =
  hmFindOrganizationByEmail (reqDto ^. email) $ \org ->
    hmCreateActionKey (org ^. organizationId) ForgottenTokenActionKey $ \actionKey -> do
      emailResult <- sendResetTokenMail (toDTO org) (actionKey ^. hash)
      case emailResult of
        Left errMessage -> return . Just $ GeneralServerError _ERROR_SERVICE_ORGANIZATION__RECOVERY_EMAIL_NOT_SENT
        _ -> return Nothing

changeOrganizationState ::
     String -> Maybe String -> OrganizationStateDTO -> AppContextM (Either AppError OrganizationDTO)
changeOrganizationState organizationId maybeHash reqDto =
  heExtractHash maybeHash $ \akHash ->
    heFindOrganizationByOrgId organizationId $ \org ->
      heGetActionKeyByHash akHash $ \actionKey -> do
        updatedOrg <- updateOrgTimestamp $ org & active .~ (reqDto ^. active)
        updateOrganization updatedOrg
        deleteActionKey (actionKey ^. hash)
        return . Right . toDTO $ updatedOrg

-- --------------------------------
-- PERMISSIONS
-- --------------------------------
checkPermissionToListOrganizations callback =
  heGetCurrentOrganization $ \currentOrg ->
    if currentOrg ^. role == AdminRole
      then callback
      else return . Left . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "List Organizations"

checkPermissionToOrganization org callback =
  heGetCurrentOrganization $ \currentOrg ->
    if currentOrg ^. role == AdminRole || (org ^. organizationId) == (currentOrg ^. organizationId)
      then callback
      else return . Left . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Detail Organization"

-- --------------------------------
-- PRIVATE
-- --------------------------------
generateNewOrgToken :: AppContextM String
generateNewOrgToken = liftIO $ generateRandomString 256

updateOrgTimestamp :: Organization -> AppContextM Organization
updateOrgTimestamp user = do
  now <- liftIO getCurrentTime
  return $ user & updatedAt .~ now

heExtractHash maybeHash callback =
  case maybeHash of
    Just hash -> callback hash
    Nothing -> return . Left . UserError $ _ERROR_SERVICE_ORGANIZATION__REQUIRED_HASH_IN_QUERY_PARAMS

-- --------------------------------
-- HELPERS
-- --------------------------------
heGetOrganizationByOrgId orgId callback = createHeeHelper (getOrganizationByOrgId orgId) callback

hmGetOrganizationByOrgId orgId callback = createHemHelper (getOrganizationByOrgId orgId) callback
