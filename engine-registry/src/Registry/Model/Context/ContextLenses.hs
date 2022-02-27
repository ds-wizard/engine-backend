module Registry.Model.Context.ContextLenses where

import Control.Lens ((&), (.~), (^.), (^?), _Just)
import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

import LensesConfig
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Shared.Constant.App
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.ContextLenses

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

instance HasCloud' ServerConfig where
  cloud' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: ServerConfig -> ServerConfigCloud
      get entity = entity ^. cloud
      set :: ServerConfig -> ServerConfigCloud -> ServerConfig
      set entity newValue = entity & cloud .~ newValue

instance HasDbPool' AppContext where
  dbPool' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> Pool Connection
      get entity = entity ^. dbPool
      set :: AppContext -> Pool Connection -> AppContext
      set entity newValue = entity & dbPool .~ newValue

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

instance HasIdentityUuid' AppContext where
  identityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> Maybe String
      get entity = entity ^. currentOrganization ^? _Just . token
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
      get entity = defaultAppUuid
      set :: AppContext -> U.UUID -> AppContext
      set entity _ = entity
