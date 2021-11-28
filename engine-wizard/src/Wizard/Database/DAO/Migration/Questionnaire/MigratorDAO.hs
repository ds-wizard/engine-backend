module Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Migration.Questionnaire.MigratorState ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Migration.Questionnaire.MigratorState

entityName = "questionnaire_migration"

pageLabel = "migrations"

findMigratorStates :: AppContextM [MigratorState]
findMigratorStates = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findMigratorStatesByOldQuestionnaireId :: String -> AppContextM [MigratorState]
findMigratorStatesByOldQuestionnaireId oldQtnUuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("old_questionnaire_uuid", oldQtnUuid)]

findMigratorStateByNewQuestionnaireId :: String -> AppContextM MigratorState
findMigratorStateByNewQuestionnaireId newQuestionnaireUuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("new_questionnaire_uuid", newQuestionnaireUuid)]

findMigratorStateByNewQuestionnaireId' :: String -> AppContextM (Maybe MigratorState)
findMigratorStateByNewQuestionnaireId' newQuestionnaireUuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("new_questionnaire_uuid", newQuestionnaireUuid)]

insertMigratorState :: MigratorState -> AppContextM Int64
insertMigratorState = createInsertFn entityName

updateMigratorStateByNewQuestionnaireId :: MigratorState -> AppContextM Int64
updateMigratorStateByNewQuestionnaireId ms = do
  appUuid <- asks _appContextAppUuid
  let sql =
        fromString
          "UPDATE questionnaire_migration SET old_questionnaire_uuid = ?, new_questionnaire_uuid = ?, resolved_question_uuids = ?, app_uuid = ? WHERE app_uuid = ? AND new_questionnaire_uuid = ?"
  let params = toRow ms ++ [toField appUuid, toField $ ms ^. newQuestionnaireUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteMigratorStates :: AppContextM Int64
deleteMigratorStates = createDeleteEntitiesFn entityName

deleteMigratorStateByNewQuestionnaireId :: String -> AppContextM Int64
deleteMigratorStateByNewQuestionnaireId newQuestionnaireUuid = do
  appUuid <- asks _appContextAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("new_questionnaire_uuid", newQuestionnaireUuid)]
