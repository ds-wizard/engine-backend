module Wizard.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.Locale.KnowledgeModelLocale ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocale

entityName = "knowledge_model_locale"

findKnowledgeModelLocalesByPackageUuid :: U.UUID -> AppContextM [KnowledgeModelLocale]
findKnowledgeModelLocalesByPackageUuid pkgUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("knowledge_model_package_uuid", U.toString pkgUuid)]

findKnowledgeModelLocaleByUuid :: U.UUID -> AppContextM KnowledgeModelLocale
findKnowledgeModelLocaleByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findKnowledgeModelLocaleByPackageUuidAndUuid :: U.UUID -> U.UUID -> AppContextM KnowledgeModelLocale
findKnowledgeModelLocaleByPackageUuidAndUuid pkgUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("knowledge_model_package_uuid", U.toString pkgUuid), ("uuid", U.toString uuid)]

findKnowledgeModelLocaleByPackageUuidAndCode' :: U.UUID -> String -> AppContextM (Maybe KnowledgeModelLocale)
findKnowledgeModelLocaleByPackageUuidAndCode' pkgUuid code = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("knowledge_model_package_uuid", U.toString pkgUuid), ("code", code)]

insertKnowledgeModelLocale :: KnowledgeModelLocale -> AppContextM Int64
insertKnowledgeModelLocale = createInsertFn entityName

deleteKnowledgeModelLocales :: AppContextM Int64
deleteKnowledgeModelLocales = createDeleteEntitiesFn entityName

deleteKnowledgeModelLocaleByUuid :: U.UUID -> AppContextM Int64
deleteKnowledgeModelLocaleByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
