module Wizard.Database.Migration.Production.Migration_0003_metricsAndPhases.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

definition = (meta, migrate)

meta =
  MigrationMeta
    {mmNumber = 3, mmName = "Metrics and Phases", mmDescription = "Move metrics and phases into Knowledge Model"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  dropMetricTable dbPool
  dropLevelTable dbPool
  updateEventsInQuestionnaires dbPool

-- ------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------
dropMetricTable dbPool = do
  let sql = "DROP TABLE IF EXISTS metric;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------
dropLevelTable dbPool = do
  let sql = "DROP TABLE IF EXISTS level;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------------------
updateEventsInQuestionnaires dbPool = do
  let sql = "SELECT uuid, events FROM questionnaire;"
  let action conn = query_ conn (fromString sql)
  qtns <- liftIO $ withResource dbPool action
  traverse_ (updateEventsInQuestionnaire dbPool) qtns
  return Nothing

updateEventsInQuestionnaire :: Pool Connection -> Questionnaire -> LoggingT IO Int64
updateEventsInQuestionnaire dbPool qtn = do
  let updatedQtn = qtn {events = fmap updateEvent (events qtn)}
  let sql = "UPDATE questionnaire SET events = ? WHERE uuid = ?"
  let action conn = execute conn (fromString sql) (toRow updatedQtn)
  liftIO $ withResource dbPool action

updateEvent :: M.Map String Value -> M.Map String Value
updateEvent = updatePhase . updateType
  where
    updateType event =
      case M.lookup "type" event of
        Just "SetLevelEvent" -> M.insert "type" "SetPhaseEvent" event
        _ -> event
    updatePhase event =
      case M.lookup "level" event of
        Just (Number 1) -> M.insert "phase" "b101f2d0-2476-452d-aa8d-95a41a02b52c" . M.delete "level" $ event
        Just (Number 2) -> M.insert "phase" "1796fa3c-9f53-475f-89ff-c66a0453c42e" . M.delete "level" $ event
        Just (Number 3) -> M.insert "phase" "adc9133d-afcd-4616-9aea-db5f475898a2" . M.delete "level" $ event
        _ -> event

data Questionnaire =
  Questionnaire
    { uuid :: U.UUID
    , events :: [M.Map String Value]
    }

instance ToRow Questionnaire where
  toRow Questionnaire {..} = [toJSONField events, toField uuid]

instance FromRow Questionnaire where
  fromRow = do
    uuid <- field
    events <- fieldWith fromJSONField
    return $ Questionnaire {..}
