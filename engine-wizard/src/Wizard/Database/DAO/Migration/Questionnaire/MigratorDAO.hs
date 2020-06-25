module Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO where

import Control.Lens ((^.))
import Data.Bson

import LensesConfig
import Shared.Database.DAO.Common
import Wizard.Database.BSON.Migration.Questionnaire.MigratorState ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Migration.Questionnaire.MigratorState

entityName = "questionnaireMigration"

collection = "questionnaireMigrations"

findMigratorStates :: AppContextM [MigratorState]
findMigratorStates = createFindEntitiesFn collection

findMigratorStatesByOldQuestionnaireId :: String -> AppContextM [MigratorState]
findMigratorStatesByOldQuestionnaireId oldQtnUuid =
  createFindEntitiesByFn collection ["oldQuestionnaireUuid" =: oldQtnUuid]

findMigratorStateByNewQuestionnaireId :: String -> AppContextM MigratorState
findMigratorStateByNewQuestionnaireId = createFindEntityByFn collection entityName "newQuestionnaireUuid"

findMigratorStateByNewQuestionnaireId' :: String -> AppContextM (Maybe MigratorState)
findMigratorStateByNewQuestionnaireId' = createFindEntityByFn' collection entityName "newQuestionnaireUuid"

insertMigratorState :: MigratorState -> AppContextM Value
insertMigratorState = createInsertFn collection

updateMigratorStateByNewQuestionnaireId :: MigratorState -> AppContextM ()
updateMigratorStateByNewQuestionnaireId state =
  createUpdateByFn collection "newQuestionnaireUuid" (state ^. newQuestionnaireUuid) state

deleteMigratorStates :: AppContextM ()
deleteMigratorStates = createDeleteEntitiesFn collection

deleteMigratorStateByNewQuestionnaireId :: String -> AppContextM ()
deleteMigratorStateByNewQuestionnaireId = createDeleteEntityByFn collection "newQuestionnaireUuid"
