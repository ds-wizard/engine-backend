module Wizard.Service.Tenant.TenantService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Crypto
import Shared.Common.Util.Uuid
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleDM
import Wizard.Api.Resource.Tenant.TenantChangeDTO
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantDetailDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigProjectDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigDM
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.Limit.LimitService
import Wizard.Service.Tenant.TenantMapper
import Wizard.Service.Tenant.TenantUtil
import Wizard.Service.Tenant.TenantValidation
import Wizard.Service.Tenant.Usage.UsageService
import qualified Wizard.Service.User.UserMapper as U_Mapper
import Wizard.Service.User.UserService
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigMailDAO
import WizardLib.Public.Database.DAO.Tenant.TenantDAO
import WizardLib.Public.Database.DAO.User.RoleDAO
import WizardLib.Public.Model.PersistentCommand.Tenant.CreateTenantCommand
import WizardLib.Public.Model.PersistentCommand.Tenant.UpdateTenantCommand
import WizardLib.Public.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Model.Tenant.Config.TenantConfigDM
import WizardLib.Public.Model.Tenant.TenantSuggestion
import WizardLib.Public.Model.User.Role

getTenantsPage :: Maybe String -> Maybe [TenantState] -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page TenantDTO)
getTenantsPage mQuery mStates mEnabled pageable sort = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  tenants <- findTenantsPage mQuery mStates mEnabled pageable sort
  traverse enhanceTenant tenants

getTenantSuggestions :: Maybe String -> AppContextM [TenantSuggestion]
getTenantSuggestions mQuery = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  findTenantSuggestions mQuery

registerOrCreateTenantByAdmin :: TenantCreateDTO -> AppContextM TenantDTO
registerOrCreateTenantByAdmin reqDto = do
  hasPermission <- hasPermission _TENANTS_MANAGE_ROLE_PERMISSION
  if hasPermission
    then createTenantByAdmin reqDto
    else registerTenant reqDto

registerTenant :: TenantCreateDTO -> AppContextM TenantDTO
registerTenant reqDto = do
  runInTransaction $ do
    validateTenantCreateDTO reqDto False
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    serverConfig <- asks serverConfig
    let tenant = fromRegisterCreateDTO reqDto uuid serverConfig now
    insertTenant tenant
    adminRole <- createAdminRole uuid now
    createLimitBundle uuid now
    userUuid <- liftIO generateUuid
    let userCreate = U_Mapper.fromTenantCreateToUserCreateDTO reqDto adminRole.uuid
    user <- createUserByAdminWithUuid userCreate userUuid tenant.uuid tenant.clientUrl True
    createConfig uuid adminRole.uuid now
    createLocale uuid now
    return $ toDTO tenant Nothing Nothing

createTenantByAdmin :: TenantCreateDTO -> AppContextM TenantDTO
createTenantByAdmin reqDto = do
  runInTransaction $ do
    checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
    validateTenantCreateDTO reqDto True
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    serverConfig <- asks serverConfig
    let tenant = fromAdminCreateDTO reqDto uuid serverConfig now
    insertTenant tenant
    adminRole <- createAdminRole uuid now
    createLimitBundle uuid now
    userUuid <- liftIO generateUuid
    userPassword <- liftIO $ generateRandomString 25
    let userCreate = U_Mapper.fromTenantCreateToUserCreateDTO (reqDto {password = userPassword}) adminRole.uuid
    user <- createUserByAdminWithUuid userCreate userUuid tenant.uuid tenant.clientUrl False
    createConfig uuid adminRole.uuid now
    createLocale uuid now
    return $ toDTO tenant Nothing Nothing

createTenantByCommand :: CreateTenantCommand -> AppContextM ()
createTenantByCommand command = do
  now <- liftIO getCurrentTime
  serverConfig <- asks serverConfig
  let tenant = fromCreateCommand command NotSeededTenantState serverConfig now now
  insertTenant tenant
  adminRole <- createAdminRoleWithUuid command.adminRoleUuid tenant.uuid command.adminRolePermissions now
  createConfig tenant.uuid adminRole.uuid now
  createLimitBundle tenant.uuid now
  createLocale tenant.uuid now
  return ()

getTenantByUuid :: U.UUID -> AppContextM TenantDetailDTO
getTenantByUuid uuid = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  tenant <- findTenantByUuid uuid
  usage <- getUsage uuid
  allUsers <- findUsersWithTenantFiltered uuid []
  let users = filter (elem _USERS_MANAGE_ROLE_PERMISSION . (.role.permissions)) allUsers
  tcLookAndFeel <- findTenantConfigLookAndFeelByUuid uuid
  let mLogoUrl = tcLookAndFeel.logoUrl
  let mPrimaryColor = tcLookAndFeel.primaryColor
  return $ toDetailDTO tenant mLogoUrl mPrimaryColor usage users

modifyTenant :: U.UUID -> TenantChangeDTO -> AppContextM Tenant
modifyTenant uuid reqDto = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  tenant <- findTenantByUuid uuid
  validateTenantChangeDTO tenant reqDto
  serverConfig <- asks serverConfig
  let updatedTenant = fromChangeDTO tenant reqDto serverConfig
  updateTenantByUuid updatedTenant

modifyTenantFromCommand :: UpdateTenantCommand -> AppContextM Tenant
modifyTenantFromCommand command =
  runInTransaction $ do
    checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
    now <- liftIO getCurrentTime
    serverConfig <- asks serverConfig
    tenant <- findTenantByUuid command.uuid
    let updatedTenant = fromUpdateCommand command tenant.state serverConfig tenant.createdAt now
    updateTenantByUuid updatedTenant
    modifyLimitBundle command.uuid command.limits
    return updatedTenant

deleteTenant :: U.UUID -> AppContextM ()
deleteTenant uuid = do
  checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
  _ <- findTenantByUuid uuid
  deleteTenantByUuid uuid
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
createAdminRole :: U.UUID -> UTCTime -> AppContextM Role
createAdminRole tenantUuid now = do
  uuid <- liftIO generateUuid
  createAdminRoleWithUuid uuid tenantUuid allRolePermissions now

createAdminRoleWithUuid :: U.UUID -> U.UUID -> [String] -> UTCTime -> AppContextM Role
createAdminRoleWithUuid uuid tenantUuid permissions now = do
  let role =
        Role
          { uuid = uuid
          , name = "Admin"
          , permissions = permissions
          , isAdmin = True
          , tenantUuid = tenantUuid
          , createdAt = now
          , updatedAt = now
          }
  insertRole role
  return role

createConfig :: U.UUID -> U.UUID -> UTCTime -> AppContextM ()
createConfig uuid defaultRoleUuid now = do
  runInTransaction $ do
    insertTenantConfigOrganization (defaultOrganization {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigOrganization)
    insertTenantConfigAuthentication (defaultAuthentication {tenantUuid = uuid, defaultRoleUuid = defaultRoleUuid, createdAt = now, updatedAt = now} :: TenantConfigAuthentication)
    insertTenantConfigPrivacyAndSupport (defaultPrivacyAndSupport {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigPrivacyAndSupport)
    insertTenantConfigDashboardAndLoginScreen (defaultDashboardAndLoginScreen {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigDashboardAndLoginScreen)
    insertTenantConfigLookAndFeel (defaultLookAndFeel {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigLookAndFeel)
    insertTenantConfigRegistry (defaultRegistry {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigRegistry)
    insertTenantConfigProject (defaultProject {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigProject)
    insertTenantConfigSubmission (defaultSubmission {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigSubmission)
    insertTenantConfigMail (defaultMail {tenantUuid = uuid, createdAt = now, updatedAt = now})
    insertTenantConfigFeatures (defaultFeatures {tenantUuid = uuid, createdAt = now, updatedAt = now})
    void $ insertTenantConfigOwl (defaultOwl {tenantUuid = uuid, createdAt = now, updatedAt = now} :: TenantConfigOwl)

createLocale :: U.UUID -> UTCTime -> AppContextM Locale
createLocale tntUuid now = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    let locale =
          localeDefault
            { uuid = uuid
            , tenantUuid = tntUuid
            , createdAt = now
            , updatedAt = now
            }
            :: Locale
    insertLocale locale
    return locale
