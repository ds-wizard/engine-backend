module Database.DAO.Migration.Questionnaire.MigratorDAO
  ( findMigratorStates
  , findMigratorStateByOldQuestionnaireId
  , findMigratorStateByNewQuestionnaireId
  , createMigratorState
  , updateMigratorStateByNewQuestionnaireId
  , deleteMigratorStates
  , deleteMigratorStateByNewQuestionnaireId
  -- Helpers
  , heFindMigratorStateByNewQuestionnaireId
  ) where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, fetch, findOne, insert, merge, save, select)

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

findMigratorStateByOldQuestionnaireId :: String -> AppContextM (Either AppError MigratorState)
findMigratorStateByOldQuestionnaireId qtnUuid = do
  let action = findOne $ select ["oldQuestionnaireUuid" =: qtnUuid] collection
  maybeState <- runDB action
  return . deserializeMaybeEntity entityName qtnUuid $ maybeState

findMigratorStateByNewQuestionnaireId :: String -> AppContextM (Either AppError MigratorState)
findMigratorStateByNewQuestionnaireId qtnUuid = do
  let action = findOne $ select ["newQuestionnaireUuid" =: qtnUuid] collection
  maybeState <- runDB action
  return . deserializeMaybeEntity entityName qtnUuid $ maybeState

createMigratorState :: MigratorState -> AppContextM Value
createMigratorState state = do
  let action = insert collection (toBSON state)
  runDB action

updateMigratorStateByNewQuestionnaireId :: MigratorState -> AppContextM ()
updateMigratorStateByNewQuestionnaireId state = do
  let action =
        fetch (select ["newQuestionnaireUuid" =: (state ^. newQuestionnaireUuid)] collection) >>=
        save collection . merge (toBSON state)
  runDB action

deleteMigratorStates :: AppContextM ()
deleteMigratorStates = do
  let action = delete $ select [] collection
  runDB action

deleteMigratorStateByNewQuestionnaireId :: String -> AppContextM ()
deleteMigratorStateByNewQuestionnaireId qtnUuid = do
  let action = delete $ select ["newQuestionnaireUuid" =: qtnUuid] collection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindMigratorStateByNewQuestionnaireId qtnUuid = createHeeHelper (findMigratorStateByNewQuestionnaireId qtnUuid)
