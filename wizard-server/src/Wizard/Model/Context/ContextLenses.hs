module Wizard.Model.Context.ContextLenses where

import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import qualified Shared.Common.Model.Context.AppContext as S_AppContext
import qualified Shared.Common.Model.Context.BaseContext as S_BaseContext
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AclContext ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext

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

instance HasField "environment'" ServerConfig String where
  getField = (.general.environment)

instance HasField "database'" ServerConfig ServerConfigDatabase where
  getField = (.database)

instance HasField "s3'" ServerConfig ServerConfigS3 where
  getField = (.s3)

instance HasField "sentry'" ServerConfig ServerConfigSentry where
  getField = (.sentry)

instance HasField "logging'" ServerConfig ServerConfigLogging where
  getField = (.logging)

instance HasField "cloud'" ServerConfig ServerConfigCloud where
  getField = (.cloud)

instance HasField "persistentCommand'" ServerConfig ServerConfigPersistentCommand where
  getField = (.persistentCommand)

instance HasField "aws'" ServerConfig ServerConfigAws where
  getField = (.aws)

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
  getField entity = fmap (U.toString . (.uuid)) $ entity.currentUser

instance HasField "identityEmail'" AppContext (Maybe String) where
  getField entity = fmap (.email) entity.currentUser

instance HasField "traceUuid'" AppContext U.UUID where
  getField = (.traceUuid)

instance HasField "tenantUuid'" AppContext U.UUID where
  getField = (.currentTenantUuid)

instance HasField "cache'" AppContext ServerCache where
  getField = (.cache)
