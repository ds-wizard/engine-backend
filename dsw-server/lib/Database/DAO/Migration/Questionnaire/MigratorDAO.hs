module Database.DAO.Migration.Questionnaire.MigratorDAO
  ( findMigratorStates
  , findMigratorStatesByOldQuestionnaireId
  , findMigratorStateByNewQuestionnaireId
  , insertMigratorState
  , updateMigratorStateByNewQuestionnaireId
  , deleteMigratorStates
  , deleteMigratorStateByNewQuestionnaireId
  -- Helpers
  , heFindMigratorStateByNewQuestionnaireId
  ) where

import Control.Lens ((^.))
import Data.Bson

import Database.BSON.Migration.Questionnaire.MigratorState ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Migration.Questionnaire.MigratorState
import Util.Helper (createHeeHelper)

entityName = "questionnaireMigration"

collection = "questionnaireMigrations"

findMigratorStates :: AppContextM (Either AppError [MigratorState])
findMigratorStates = createFindEntitiesFn collection

findMigratorStatesByOldQuestionnaireId :: String -> AppContextM (Either AppError [MigratorState])
findMigratorStatesByOldQuestionnaireId oldQtnUuid =
  createFindEntitiesByFn collection ["oldQuestionnaireUuid" =: oldQtnUuid]

findMigratorStateByNewQuestionnaireId :: String -> AppContextM (Either AppError MigratorState)
findMigratorStateByNewQuestionnaireId = createFindEntityByFn collection entityName "newQuestionnaireUuid"

insertMigratorState :: MigratorState -> AppContextM Value
insertMigratorState = createInsertFn collection

updateMigratorStateByNewQuestionnaireId :: MigratorState -> AppContextM ()
updateMigratorStateByNewQuestionnaireId state =
  createUpdateByFn collection "newQuestionnaireUuid" (state ^. newQuestionnaireUuid) state

deleteMigratorStates :: AppContextM ()
deleteMigratorStates = createDeleteEntitiesFn collection

deleteMigratorStateByNewQuestionnaireId :: String -> AppContextM ()
deleteMigratorStateByNewQuestionnaireId = createDeleteEntityByFn collection "newQuestionnaireUuid"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindMigratorStateByNewQuestionnaireId qtnUuid = createHeeHelper (findMigratorStateByNewQuestionnaireId qtnUuid)
