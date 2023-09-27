module Wizard.Service.Tenant.TenantService where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Constant.User
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Crypto
import Shared.Common.Util.Uuid
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleDM
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import qualified Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper as PCM
import Wizard.Api.Resource.Tenant.TenantChangeDTO
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantDetailDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.TenantConfigDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.Tenant.TenantPlanDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.PersistentCommand.Tenant.ImportDefaultDataCommand
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

getTenantsPage :: Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page TenantDTO)
getTenantsPage mQuery mEnabled pageable sort = do
  checkPermission _TENANT_PERM
  tenants <- findTenantsPage mQuery mEnabled pageable sort
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
    createSeederPersistentCommand uuid user.uuid now
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
    createSeederPersistentCommand uuid user.uuid now
    return $ toDTO tenant Nothing Nothing

createTenantByCommand :: CreateOrUpdateTenantCommand -> AppContextM ()
createTenantByCommand command = do
  now <- liftIO getCurrentTime
  serverConfig <- asks serverConfig
  let tenant = fromCommand command serverConfig now
  insertTenant tenant
  createConfig tenant.uuid now
  createLimitBundle tenant.uuid now
  createLocale tenant.uuid now
  createSeederPersistentCommand tenant.uuid systemUserUuid now
  return ()

getTenantByUuid :: U.UUID -> AppContextM TenantDetailDTO
getTenantByUuid uuid = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid uuid
  plans <- findPlansForTenantUuid uuid
  usage <- getUsage uuid
  users <- findUsersWithTenantFiltered uuid [("role", _USER_ROLE_ADMIN)]
  tenantConfig <- getTenantConfigByUuid tenant.uuid
  let mLogoUrl = tenantConfig.lookAndFeel.logoUrl
  let mPrimaryColor = tenantConfig.lookAndFeel.primaryColor
  return $ toDetailDTO tenant mLogoUrl mPrimaryColor plans usage users

modifyTenant :: U.UUID -> TenantChangeDTO -> AppContextM Tenant
modifyTenant uuid reqDto = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid uuid
  validateTenantChangeDTO tenant reqDto
  serverConfig <- asks serverConfig
  let updatedTenant = fromChangeDTO tenant reqDto serverConfig
  updateTenantByUuid updatedTenant

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

createSeederPersistentCommand :: U.UUID -> U.UUID -> UTCTime -> AppContextM (PersistentCommand U.UUID)
createSeederPersistentCommand tntUuid createdBy now =
  runInTransaction $ do
    pUuid <- liftIO generateUuid
    let command =
          PCM.toPersistentCommand
            pUuid
            "data_seeder"
            "importDefaultData"
            (BSL.unpack . encode $ ImportDefaultDataCommand tntUuid)
            1
            False
            Nothing
            tntUuid
            (Just createdBy)
            now
    insertPersistentCommand command
    return command
