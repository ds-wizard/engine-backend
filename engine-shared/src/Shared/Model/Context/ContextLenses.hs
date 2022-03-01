module Shared.Model.Context.ContextLenses where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import Data.Pool (Pool)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

import LensesConfig
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.AppContext
import Shared.Model.Context.BaseContext

class (HasS3' sc, HasSentry' sc, HasCloud' sc) =>
      HasServerConfig' entity sc
  | entity -> sc
  where
  serverConfig' :: Functor f => (sc -> f sc) -> entity -> f entity

class HasS3' entity where
  s3' :: Functor f => (ServerConfigS3 -> f ServerConfigS3) -> entity -> f entity

class HasSentry' entity where
  sentry' :: Functor f => (ServerConfigSentry -> f ServerConfigSentry) -> entity -> f entity

class HasCloud' entity where
  cloud' :: Functor f => (ServerConfigCloud -> f ServerConfigCloud) -> entity -> f entity

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

class HasBuildInfoConfig' entity where
  buildInfoConfig' :: Functor f => (BuildInfoConfig -> f BuildInfoConfig) -> entity -> f entity

class HasIdentityUuid' entity where
  identityUuid' :: Functor f => (Maybe String -> f (Maybe String)) -> entity -> f entity

instance HasIdentityUuid' AppContext where
  identityUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> Maybe String
      get entity = entity ^. identityUuid
      set :: AppContext -> Maybe String -> AppContext
      set entity newValue = entity & identityUuid .~ newValue

class HasTraceUuid' entity where
  traceUuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

instance HasTraceUuid' AppContext where
  traceUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> U.UUID
      get entity = entity ^. traceUuid
      set :: AppContext -> U.UUID -> AppContext
      set entity newValue = entity & traceUuid .~ newValue

class HasAppUuid' entity where
  appUuid' :: Functor f => (U.UUID -> f U.UUID) -> entity -> f entity

instance HasAppUuid' AppContext where
  appUuid' convert entity = fmap (set entity) (convert . get $ entity)
    where
      get :: AppContext -> U.UUID
      get entity = entity ^. appUuid
      set :: AppContext -> U.UUID -> AppContext
      set entity newValue = entity & appUuid .~ newValue
