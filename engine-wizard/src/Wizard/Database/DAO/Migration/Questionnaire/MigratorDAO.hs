module Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
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

import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper)
import Wizard.Database.BSON.Migration.Questionnaire.MigratorState ()
import Wizard.Database.DAO.Common
import Wizard.LensesConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.Questionnaire.MigratorState

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
