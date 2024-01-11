module Registry.Model.Context.ContextLenses where

import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import RegistryLib.Model.Organization.Organization
import Shared.Common.Constant.Tenant
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.Environment
import Shared.Common.Model.Config.ServerConfig
import qualified Shared.Common.Model.Context.AppContext as S_AppContext
import qualified Shared.Common.Model.Context.BaseContext as S_BaseContext
import Shared.Common.Service.Acl.AclService

instance S_BaseContext.BaseContextType BaseContext ServerConfig

instance S_BaseContext.BaseContextC BaseContext ServerConfig BaseContextM

instance S_AppContext.AppContextType AppContext ServerConfig

instance S_AppContext.AppContextC AppContext ServerConfig AppContextM

instance HasField "serverConfig'" AppContext ServerConfig where
  getField = (.serverConfig)

instance HasField "serverConfig'" BaseContext ServerConfig where
  getField = (.serverConfig)

instance HasField "serverPort'" ServerConfig Int where
  getField = (.general.serverPort)

instance HasField "environment'" ServerConfig Environment where
  getField = (.general.environment)

instance HasField "s3'" ServerConfig ServerConfigS3 where
  getField = (.s3)

instance HasField "sentry'" ServerConfig ServerConfigSentry where
  getField = (.sentry)

instance HasField "cloud'" ServerConfig ServerConfigCloud where
  getField = (.cloud)

instance HasField "persistentCommand'" ServerConfig ServerConfigPersistentCommand where
  getField = (.persistentCommand)

instance HasField "dbPool'" AppContext (Pool Connection) where
  getField = (.dbPool)

instance HasField "dbPool'" BaseContext (Pool Connection) where
  getField = (.dbPool)

instance HasField "dbConnection'" AppContext (Maybe Connection) where
  getField = (.dbConnection)

instance HasField "s3Client'" AppContext MinioConn where
  getField = (.s3Client)

instance HasField "s3Client'" BaseContext MinioConn where
  getField = (.s3Client)

instance HasField "httpClientManager'" AppContext Manager where
  getField = (.httpClientManager)

instance HasField "httpClientManager'" BaseContext Manager where
  getField = (.httpClientManager)

instance HasField "buildInfoConfig'" AppContext BuildInfoConfig where
  getField = (.buildInfoConfig)

instance HasField "buildInfoConfig'" BaseContext BuildInfoConfig where
  getField = (.buildInfoConfig)

instance HasField "identity'" AppContext (Maybe String) where
  getField entity = fmap (.token) entity.currentOrganization

instance HasField "identityEmail'" AppContext (Maybe String) where
  getField entity = fmap (.email) entity.currentOrganization

instance HasField "traceUuid'" AppContext U.UUID where
  getField = (.traceUuid)

instance HasField "tenantUuid'" AppContext U.UUID where
  getField entity = defaultTenantUuid

instance AclContext AppContextM where
  checkPermission perm = return ()
