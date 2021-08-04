module Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO where

import Control.Lens ((^.))
import Data.String
import qualified Data.UUID as U
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
import Wizard.Util.Logger

entityName = "questionnaire_migration"

pageLabel = "migrations"

findMigratorStates :: AppContextM [MigratorState]
findMigratorStates = createFindEntitiesFn entityName

findMigratorStatesByOldQuestionnaireId :: String -> AppContextM [MigratorState]
findMigratorStatesByOldQuestionnaireId oldQtnUuid =
  createFindEntitiesByFn entityName [("old_questionnaire_uuid", oldQtnUuid)]

findMigratorStateByNewQuestionnaireId :: String -> AppContextM MigratorState
findMigratorStateByNewQuestionnaireId = createFindEntityByFn entityName "new_questionnaire_uuid"

findMigratorStateByNewQuestionnaireId' :: String -> AppContextM (Maybe MigratorState)
findMigratorStateByNewQuestionnaireId' = createFindEntityByFn' entityName "new_questionnaire_uuid"

insertMigratorState :: MigratorState -> AppContextM Int64
insertMigratorState = createInsertFn entityName

updateMigratorStateByNewQuestionnaireId :: MigratorState -> AppContextM Int64
updateMigratorStateByNewQuestionnaireId ms = do
  let params = toRow ms ++ [toField . U.toText $ ms ^. newQuestionnaireUuid]
  let sql =
        "UPDATE questionnaire_migration SET old_questionnaire_uuid = ?, new_questionnaire_uuid = ?, resolved_question_uuids = ? WHERE new_questionnaire_uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

deleteMigratorStates :: AppContextM Int64
deleteMigratorStates = createDeleteEntitiesFn entityName

deleteMigratorStateByNewQuestionnaireId :: String -> AppContextM Int64
deleteMigratorStateByNewQuestionnaireId = createDeleteEntityByFn entityName "new_questionnaire_uuid"
