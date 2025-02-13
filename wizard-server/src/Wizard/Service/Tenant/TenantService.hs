module Wizard.Service.Tenant.TenantService where

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
import Wizard.Database.DAO.Tenant.TenantConfigDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigDM
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import Wizard.Service.Tenant.TenantMapper
import Wizard.Service.Tenant.TenantUtil
import Wizard.Service.Tenant.TenantValidation
import Wizard.Service.Tenant.Usage.UsageService
import qualified Wizard.Service.User.UserMapper as U_Mapper
import Wizard.Service.User.UserService
import WizardLib.Public.Model.PersistentCommand.Tenant.CreateOrUpdateTenantCommand

getTenantsPage :: Maybe String -> Maybe [TenantState] -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page TenantDTO)
getTenantsPage mQuery mStates mEnabled pageable sort = do
  checkPermission _TENANT_PERM
  tenants <- findTenantsPage mQuery mStates mEnabled pageable sort
  traverse enhanceTenant tenants

registerTenant :: TenantCreateDTO -> AppContextM TenantDTO
registerTenant reqDto = do
  runInTransaction $ do
    validateTenantCreateDTO reqDto False
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    serverConfig <- asks serverConfig
    let tenant = fromRegisterCreateDTO reqDto uuid serverConfig now
    insertTenant tenant
    userUuid <- liftIO generateUuid
    let userCreate = U_Mapper.fromTenantCreateToUserCreateDTO reqDto
    user <- createUserByAdminWithUuid userCreate userUuid tenant.uuid tenant.clientUrl True
    createConfig uuid now
    createLimitBundle uuid now
    createLocale uuid now
    return $ toDTO tenant Nothing Nothing

createTenantByAdmin :: TenantCreateDTO -> AppContextM TenantDTO
createTenantByAdmin reqDto = do
  runInTransaction $ do
    checkPermission _TENANT_PERM
    validateTenantCreateDTO reqDto True
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    serverConfig <- asks serverConfig
    let tenant = fromAdminCreateDTO reqDto uuid serverConfig now
    insertTenant tenant
    userUuid <- liftIO generateUuid
    userPassword <- liftIO $ generateRandomString 25
    let userCreate = U_Mapper.fromTenantCreateToUserCreateDTO (reqDto {password = userPassword})
    user <- createUserByAdminWithUuid userCreate userUuid tenant.uuid tenant.clientUrl False
    createConfig uuid now
    createLimitBundle uuid now
    createLocale uuid now
    return $ toDTO tenant Nothing Nothing

createTenantByCommand :: CreateOrUpdateTenantCommand -> AppContextM ()
createTenantByCommand command = do
  now <- liftIO getCurrentTime
  serverConfig <- asks serverConfig
  let tenant = fromCommand command NotSeededTenantState serverConfig now now
  insertTenant tenant
  createConfig tenant.uuid now
  createLimitBundle tenant.uuid now
  createLocale tenant.uuid now
  return ()

getTenantByUuid :: U.UUID -> AppContextM TenantDetailDTO
getTenantByUuid uuid = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid uuid
  usage <- getUsage uuid
  users <- findUsersWithTenantFiltered uuid [("role", _USER_ROLE_ADMIN)]
  tenantConfig <- getTenantConfigByUuid tenant.uuid
  let mLogoUrl = tenantConfig.lookAndFeel.logoUrl
  let mPrimaryColor = tenantConfig.lookAndFeel.primaryColor
  return $ toDetailDTO tenant mLogoUrl mPrimaryColor usage users

modifyTenant :: U.UUID -> TenantChangeDTO -> AppContextM Tenant
modifyTenant uuid reqDto = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid uuid
  validateTenantChangeDTO tenant reqDto
  serverConfig <- asks serverConfig
  let updatedTenant = fromChangeDTO tenant reqDto serverConfig
  updateTenantByUuid updatedTenant

modifyTenantFromCommand :: CreateOrUpdateTenantCommand -> AppContextM Tenant
modifyTenantFromCommand command =
  runInTransaction $ do
    checkPermission _TENANT_PERM
    now <- liftIO getCurrentTime
    serverConfig <- asks serverConfig
    tenant <- findTenantByUuid command.uuid
    let updatedTenant = fromCommand command tenant.state serverConfig tenant.createdAt now
    updateTenantByUuid updatedTenant
    modifyLimitBundle command.uuid command.limits
    return updatedTenant

deleteTenant :: U.UUID -> AppContextM ()
deleteTenant uuid = do
  checkPermission _TENANT_PERM
  _ <- findTenantByUuid uuid
  deleteTenantByUuid uuid
  return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
createConfig :: U.UUID -> UTCTime -> AppContextM TenantConfig
createConfig tntUuid now = do
  runInTransaction $ do
    let tenantConfig =
          defaultTenantConfig
            { uuid = tntUuid
            , createdAt = now
            , updatedAt = now
            }
          :: TenantConfig
    insertTenantConfig tenantConfig
    return tenantConfig

createLocale :: U.UUID -> UTCTime -> AppContextM Locale
createLocale tntUuid now = do
  runInTransaction $ do
    let locale =
          localeDefault
            { tenantUuid = tntUuid
            , createdAt = now
            , updatedAt = now
            }
          :: Locale
    insertLocale locale
    return locale
