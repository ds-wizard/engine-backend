module Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO where

import Control.Monad.Reader (asks)
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageEvent ()
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageRawEvent ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageRawEvent

entityName = "knowledge_model_package_event"

findPackageEvents :: AppContextC s sc m => String -> m [KnowledgeModelPackageEvent]
findPackageEvents pId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("package_id", pId)] [Sort "created_at" Ascending]

findPackageRawEvents :: AppContextC s sc m => String -> m [KnowledgeModelPackageRawEvent]
findPackageRawEvents pId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("package_id", pId)] [Sort "created_at" Ascending]

insertPackageEvent :: AppContextC s sc m => KnowledgeModelPackageEvent -> m Int64
insertPackageEvent = createInsertFn entityName

insertPackageRawEvent :: AppContextC s sc m => KnowledgeModelPackageRawEvent -> m Int64
insertPackageRawEvent = createInsertFn entityName

deletePackageEventsById :: AppContextC s sc m => String -> m Int64
deletePackageEventsById id = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("knowledge_model_package_id", id)]
