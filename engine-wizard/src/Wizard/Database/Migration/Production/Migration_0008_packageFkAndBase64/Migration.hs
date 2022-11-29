module Wizard.Database.Migration.Production.Migration_0008_packageFkAndBase64.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Int
import qualified Data.Map.Strict as M
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.UUID as U
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField hiding (Array)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 8
    , mmName = "Package Foreign Key and Base64"
    , mmDescription = "Remove foreign key from package and base64 for secret data"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  dropForeignKeys dbPool
  migrateToBase64 dbPool

-- ------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------
dropForeignKeys dbPool = do
  dropForkOfPackageIdFk dbPool
  dropMergeCheckpointPackageIdFk dbPool

-- ------------------------------------------------------------------------------------------------------------
dropForkOfPackageIdFk dbPool = do
  let sql = "ALTER TABLE package DROP CONSTRAINT package_fork_of_package_id_fk;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

dropMergeCheckpointPackageIdFk dbPool = do
  let sql = "ALTER TABLE package DROP CONSTRAINT package_merge_checkpoint_package_id_fk;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------
migrateToBase64 dbPool = do
  updateSubmissionPropsInUsers dbPool
  updateAppConfig dbPool

-- ------------------------------------------------------------------------------------------------------------
updateSubmissionPropsInUsers dbPool = do
  let sql = "SELECT uuid, submissions_props FROM user_entity;"
  let action conn = query_ conn (fromString sql)
  users <- liftIO $ withResource dbPool action
  traverse_ (updateSubmissionPropsInUser dbPool) users
  return Nothing

updateSubmissionPropsInUser :: Pool Connection -> User -> LoggingT IO Int64
updateSubmissionPropsInUser dbPool user = do
  let updatedUser = user {submissionsProps = fmap updateSubmissionProp (submissionsProps user)}
  let sql = "UPDATE user_entity SET submissions_props = ? WHERE uuid = ?"
  let action conn = execute conn (fromString sql) (toRow updatedUser)
  liftIO $ withResource dbPool action

updateSubmissionProp :: M.Map String Value -> M.Map String Value
updateSubmissionProp prop =
  case M.lookup "values" prop of
    Just (Object values) -> M.insert "values" (Object . KM.map (\(String a) -> convertToBase64 a) $ values) prop
    _ -> prop

data User = User
  { uuid :: U.UUID
  , submissionsProps :: [M.Map String Value]
  }

instance ToRow User where
  toRow User {..} = [toJSONField submissionsProps, toField uuid]

instance FromRow User where
  fromRow = do
    uuid <- field
    submissionsProps <- fieldWith fromJSONField
    return $ User {..}

-- ------------------------------------------------------------------------------------------------------------
updateAppConfig dbPool = do
  let sql = "SELECT id, authentication, registry, questionnaire FROM app_config;"
  let action conn = query_ conn (fromString sql)
  appConfigs <- liftIO $ withResource dbPool action
  traverse_ (updateFieldsInAppConfig dbPool) appConfigs
  return Nothing

updateFieldsInAppConfig :: Pool Connection -> AppConfig -> LoggingT IO Int64
updateFieldsInAppConfig dbPool appConfig = do
  let updatedAppConfig =
        appConfig
          { authentication = updateAuthentication (authentication appConfig)
          , registry = updateRegistry (registry appConfig)
          , questionnaire = updateQuestionnaire (questionnaire appConfig)
          }
  let sql = "UPDATE app_config SET authentication = ?, registry = ?, questionnaire = ? WHERE id = ?"
  let action conn = execute conn (fromString sql) (toRow updatedAppConfig)
  liftIO $ withResource dbPool action

updateAuthentication :: M.Map String Value -> M.Map String Value
updateAuthentication = updateEntity
  where
    updateEntity entity =
      case M.lookup "external" entity of
        Just (Object external) -> M.insert "external" (Object . updateExternal $ external) entity
        _ -> entity
    updateExternal external =
      case KM.lookup "services" external of
        Just (Array services) -> KM.insert "services" (Array . fmap updateService $ services) external
        _ -> external
    updateService (Object service) = Object . updateClientSecret . updateClientId $ service
    updateClientId service =
      case KM.lookup "clientId" service of
        Just (String clientId) -> KM.insert "clientId" (convertToBase64 clientId) service
        _ -> service
    updateClientSecret service =
      case KM.lookup "clientSecret" service of
        Just (String clientSecret) -> KM.insert "clientSecret" (convertToBase64 clientSecret) service
        _ -> service

updateRegistry :: M.Map String Value -> M.Map String Value
updateRegistry entity =
  case M.lookup "token" entity of
    Just (String token) -> M.insert "token" (convertToBase64 token) entity
    _ -> entity

updateQuestionnaire :: M.Map String Value -> M.Map String Value
updateQuestionnaire = updateEntity
  where
    updateEntity entity =
      case M.lookup "feedback" entity of
        Just (Object feedback) -> M.insert "feedback" (Object . updateFeedback $ feedback) entity
        _ -> entity
    updateFeedback feedback =
      case KM.lookup "token" feedback of
        Just (String token) -> KM.insert "token" (convertToBase64 token) feedback
        _ -> feedback

data AppConfig = AppConfig
  { id :: Int
  , authentication :: M.Map String Value
  , registry :: M.Map String Value
  , questionnaire :: M.Map String Value
  }

instance ToRow AppConfig where
  toRow AppConfig {..} = [toJSONField authentication, toJSONField registry, toJSONField questionnaire, toField id]

instance FromRow AppConfig where
  fromRow = do
    id <- field
    authentication <- fieldWith fromJSONField
    registry <- fieldWith fromJSONField
    questionnaire <- fieldWith fromJSONField
    return $ AppConfig {..}

convertToBase64 = String . T.pack . BS.unpack . B64.encodeBase64' . BS.pack . T.unpack
