module Shared.Model.Context.ContextLenses where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.Persist.MongoDB (ConnectionPool)
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

import LensesConfig
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.AppContext
import Shared.Model.Context.BaseContext

class HasS3' sc =>
      HasServerConfig' entity sc
  | entity -> sc
  where
  serverConfig' :: Functor f => (sc -> f sc) -> entity -> f entity

class HasS3' entity where
  s3' :: Functor f => (ServerConfigS3 -> f ServerConfigS3) -> entity -> f entity

class HasPool' entity where
  pool' :: Functor f => (ConnectionPool -> f ConnectionPool) -> entity -> f entity

instance HasPool' AppContext where
  pool' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> ConnectionPool
      get entity = entity ^. pool
      set :: AppContext -> ConnectionPool -> AppContext
      set entity newValue = entity & pool .~ newValue

instance HasPool' BaseContext where
  pool' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: BaseContext -> ConnectionPool
      get entity = entity ^. pool
      set :: BaseContext -> ConnectionPool -> BaseContext
      set entity newValue = entity & pool .~ newValue

class HasDbPool' entity where
  dbPool' :: Functor f => (Pool Connection -> f (Pool Connection)) -> entity -> f entity

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

class HasS3Client' entity where
  s3Client' :: Functor f => (MinioConn -> f MinioConn) -> entity -> f entity

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

class HasLocalization' entity where
  localization' :: Functor f => (M.Map String String -> f (M.Map String String)) -> entity -> f entity

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

class HasTraceUuid' entity where
  traceUuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

instance HasTraceUuid' AppContext where
  traceUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> U.UUID
      get entity = entity ^. traceUuid
      set :: AppContext -> U.UUID -> AppContext
      set entity newValue = entity & traceUuid .~ newValue
