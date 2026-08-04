module Wizard.Service.OpenId.Client.Definition.OpenIdClientDefinitionService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.Uuid
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import WizardLib.Public.Database.DAO.OpenId.OpenIdClientDefinitionDAO
import WizardLib.Public.Model.OpenId.OpenIdClientSimple
import WizardLib.Public.Service.OpenId.Client.Definition.OpenIdClientDefinitionMapper

getOpenIdClientDefinitions :: AppContextM [OpenIdClientSimple]
getOpenIdClientDefinitions = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  openIdClients <- findOpenIdClientDefinitions
  return . fmap toSimple $ openIdClients

getOpenIdClientDefinitionByUuid :: U.UUID -> AppContextM OpenIdClientDetailDTO
getOpenIdClientDefinitionByUuid uuid = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  openIdClient <- findOpenIdClientDefinitionByUuid uuid
  return $ toDetailDTO openIdClient

createOpenIdClientDefinition :: OpenIdClientChangeDTO -> AppContextM OpenIdClientDetailDTO
createOpenIdClientDefinition reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    uuid <- liftIO generateUuid
    tenantUuid <- asks currentTenantUuid
    now <- liftIO getCurrentTime
    let openIdClient = fromCreateDTO reqDto uuid tenantUuid now
    void $ insertOpenIdClientDefinition openIdClient
    return $ toDetailDTO openIdClient

modifyOpenIdClientDefinition :: U.UUID -> OpenIdClientChangeDTO -> AppContextM OpenIdClientDetailDTO
modifyOpenIdClientDefinition uuid reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    openIdClient <- findOpenIdClientDefinitionByUuid uuid
    now <- liftIO getCurrentTime
    let updatedOpenIdClient = fromChangeDTO openIdClient reqDto now
    void $ updateOpenIdClientDefinition updatedOpenIdClient
    return $ toDetailDTO updatedOpenIdClient

deleteOpenIdClientDefinition :: U.UUID -> AppContextM ()
deleteOpenIdClientDefinition uuid =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    _ <- findOpenIdClientDefinitionByUuid uuid
    deleteOpenIdClientDefinitionByUuid uuid
