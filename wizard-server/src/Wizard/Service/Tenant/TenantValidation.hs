module Wizard.Service.Tenant.TenantValidation where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import GHC.Unicode (isAlphaNum)
import Text.Regex (matchRegex, mkRegex)

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Tenant.TenantChangeDTO
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Localization.Messages.Public
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Common

validateTenantCreateDTO :: TenantCreateDTO -> Bool -> AppContextM ()
validateTenantCreateDTO reqDto isAdmin = do
  unless isAdmin validatePublicRegistrationEnabled
  validateTenantId reqDto.tenantId

validatePublicRegistrationEnabled :: AppContextM ()
validatePublicRegistrationEnabled = checkIfServerFeatureIsEnabled "Tenant Registration" (\s -> s.cloud.publicRegistrationEnabled)

validateTenantId :: String -> AppContextM ()
validateTenantId tenantId = do
  validateTenantIdFormat tenantId
  validateTenantIdUniqueness tenantId

validateTenantChangeDTO :: Tenant -> TenantChangeDTO -> AppContextM ()
validateTenantChangeDTO tenant reqDto = do
  validateTenantIdFormat reqDto.tenantId
  when (tenant.tenantId /= reqDto.tenantId) (validateTenantIdUniqueness reqDto.tenantId)

validateTenantIdFormat :: String -> AppContextM ()
validateTenantIdFormat tenantId = forM_ (isValidTenantIdFormat tenantId) throwError

isValidTenantIdFormat :: String -> Maybe AppError
isValidTenantIdFormat tenantId =
  if not (null tenantId) && isAlphaNum (head tenantId) && isAlphaNum (last tenantId) && isJust (matchRegex validationRegex tenantId)
    then Nothing
    else Just $ ValidationError [] (M.singleton "tenantId" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS tenantId])
  where
    validationRegex = mkRegex "^[a-z0-9-]+$"

validateTenantIdUniqueness :: String -> AppContextM ()
validateTenantIdUniqueness tenantId = do
  tenants <- findTenants
  let usedTenantIds = fmap (.tenantId) tenants ++ forbiddenTenantIds
  when
    (tenantId `elem` usedTenantIds)
    (throwError . ValidationError [] $ M.singleton "tenantId" [_ERROR_VALIDATION__TENANT_ID_UNIQUENESS])

forbiddenTenantIds =
  [ "app"
  , "chronograf"
  , "cloud"
  , "czech"
  , "dashboard"
  , "datenzee"
  , "dev"
  , "docker"
  , "e2e"
  , "files"
  , "fujtajbl"
  , "grafana"
  , "ideas"
  , "integrations"
  , "keyclock"
  , "kibana"
  , "ldap"
  , "mockserver"
  , "n8n"
  , "ppe"
  , "provisioning"
  , "rabbitmq"
  , "registry"
  , "registry"
  , "registry-ppe"
  , "registry-staging"
  , "registry-test"
  , "retro"
  , "s3"
  , "staging"
  , "status"
  , "storage-costs-evaluator"
  , "submit"
  , "swarmpit"
  , "www"
  ]
