module Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageEvent ()
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageRawEvent ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageRawEvent

entityName = "knowledge_model_package_event"

findPackageEvents :: AppContextC s sc m => U.UUID -> m [KnowledgeModelPackageEvent]
findPackageEvents pkgUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("package_uuid", U.toString pkgUuid)] [Sort "created_at" Ascending]

findPackageRawEvents :: AppContextC s sc m => U.UUID -> m [KnowledgeModelPackageRawEvent]
findPackageRawEvents pkgUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("package_uuid", U.toString pkgUuid)] [Sort "created_at" Ascending]

insertPackageEvent :: AppContextC s sc m => KnowledgeModelPackageEvent -> m Int64
insertPackageEvent = createInsertFn entityName

insertPackageRawEvent :: AppContextC s sc m => KnowledgeModelPackageRawEvent -> m Int64
insertPackageRawEvent = createInsertFn entityName

deletePackageEventsByPackageUuid :: AppContextC s sc m => U.UUID -> m Int64
deletePackageEventsByPackageUuid pkgUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("package_uuid", U.toString pkgUuid)]
