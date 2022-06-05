module Wizard.Model.Context.ContextLenses where

import Control.Lens ((&), (.~), (^.), (^?), _Just)
import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

import LensesConfig
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.ContextLenses
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext

instance HasServerConfig' AppContext ServerConfig where
  serverConfig' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> ServerConfig
      get entity = entity ^. serverConfig
      set :: AppContext -> ServerConfig -> AppContext
      set entity newValue = entity & serverConfig .~ newValue

instance HasServerConfig' BaseContext ServerConfig where
  serverConfig' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: BaseContext -> ServerConfig
      get entity = entity ^. serverConfig
      set :: BaseContext -> ServerConfig -> BaseContext
      set entity newValue = entity & serverConfig .~ newValue

instance HasS3' ServerConfig where
  s3' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: ServerConfig -> ServerConfigS3
      get entity = entity ^. s3
      set :: ServerConfig -> ServerConfigS3 -> ServerConfig
      set entity newValue = entity & s3 .~ newValue

instance HasSentry' ServerConfig where
  sentry' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: ServerConfig -> ServerConfigSentry
      get entity = entity ^. sentry
      set :: ServerConfig -> ServerConfigSentry -> ServerConfig
      set entity newValue = entity & sentry .~ newValue

instance HasCloud' ServerConfig where
  cloud' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: ServerConfig -> ServerConfigCloud
      get entity = entity ^. cloud
      set :: ServerConfig -> ServerConfigCloud -> ServerConfig
      set entity newValue = entity & cloud .~ newValue

instance HasDbConnection' AppContext where
  dbConnection' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> Connection
      get entity = entity ^. dbConnection
      set :: AppContext -> Connection -> AppContext
      set entity newValue = entity & dbConnection .~ newValue

instance HasDbPool' BaseContext where
  dbPool' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: BaseContext -> Pool Connection
      get entity = entity ^. dbPool
      set :: BaseContext -> Pool Connection -> BaseContext
      set entity newValue = entity & dbPool .~ newValue

instance HasS3Client' AppContext where
  s3Client' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> MinioConn
      get entity = entity ^. s3Client
      set :: AppContext -> MinioConn -> AppContext
      set entity newValue = entity & s3Client .~ newValue

instance HasS3Client' BaseContext where
  s3Client' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: BaseContext -> MinioConn
      get entity = entity ^. s3Client
      set :: BaseContext -> MinioConn -> BaseContext
      set entity newValue = entity & s3Client .~ newValue

instance HasLocalization' AppContext where
  localization' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> M.Map String String
      get entity = entity ^. localization
      set :: AppContext -> M.Map String String -> AppContext
      set entity newValue = entity & localization .~ newValue

instance HasLocalization' BaseContext where
  localization' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: BaseContext -> M.Map String String
      get entity = entity ^. localization
      set :: BaseContext -> M.Map String String -> BaseContext
      set entity newValue = entity & localization .~ newValue

instance HasBuildInfoConfig' AppContext where
  buildInfoConfig' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> BuildInfoConfig
      get entity = entity ^. buildInfoConfig
      set :: AppContext -> BuildInfoConfig -> AppContext
      set entity newValue = entity & buildInfoConfig .~ newValue

instance HasBuildInfoConfig' BaseContext where
  buildInfoConfig' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: BaseContext -> BuildInfoConfig
      get entity = entity ^. buildInfoConfig
      set :: BaseContext -> BuildInfoConfig -> BaseContext
      set entity newValue = entity & buildInfoConfig .~ newValue

instance HasIdentityUuid' AppContext where
  identityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> Maybe String
      get entity = fmap U.toString $ entity ^. currentUser ^? _Just . uuid
      set :: AppContext -> Maybe String -> AppContext
      set entity newValue = entity

instance HasTraceUuid' AppContext where
  traceUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> U.UUID
      get entity = entity ^. traceUuid
      set :: AppContext -> U.UUID -> AppContext
      set entity newValue = entity & traceUuid .~ newValue

instance HasAppUuid' AppContext where
  appUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> U.UUID
      get entity = entity ^. appUuid
      set :: AppContext -> U.UUID -> AppContext
      set entity newValue = entity & appUuid .~ newValue
