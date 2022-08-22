module Wizard.Database.DAO.QuestionnaireImporter.QuestionnaireImporterDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.String (fromString)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.QuestionnaireImporter.QuestionnaireImporter ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter

entityName = "questionnaire_importer"

pageLabel = "questionnaireImporters"

findQuestionnaireImportersPage ::
     Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> AppContextM (Page QuestionnaireImporter)
findQuestionnaireImportersPage mOrganizationId mImporterId mQuery mEnabled pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "importer_id"
    mQuery
    mEnabled
    mOrganizationId
    mImporterId

findQuestionnaireImporterById :: String -> AppContextM QuestionnaireImporter
findQuestionnaireImporterById qiId = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", qiId)]

insertQuestionnaireImporter :: QuestionnaireImporter -> AppContextM Int64
insertQuestionnaireImporter = createInsertFn entityName

updateQuestionnaireImporterById :: QuestionnaireImporter -> AppContextM Int64
updateQuestionnaireImporterById importer = do
  appUuid <- asks _appContextAppUuid
  let sql =
        fromString
          "UPDATE questionnaire_importer SET id = ?, name = ?, organization_id = ?, importer_id = ?, version = ?, metamodel_version = ?, description = ?, readme = ?, license = ?, allowed_packages = ?, url = ?, enabled = ?, app_uuid = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND id = ?"
  let params = toRow importer ++ [toField appUuid, toField $ importer ^. qiId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateQuestionnaireImporterPasswordById :: String -> Bool -> UTCTime -> AppContextM Int64
updateQuestionnaireImporterPasswordById qiId enabled uUpdatedAt = do
  appUuid <- asks _appContextAppUuid
  let sql = fromString "UPDATE questionnaire_importer SET enabled = ?, updated_at = ? WHERE app_uuid = ? AND uuid = ?"
  let params = [toField enabled, toField uUpdatedAt, toField appUuid, toField qiId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteQuestionnaireImporters :: AppContextM Int64
deleteQuestionnaireImporters = createDeleteEntitiesFn entityName

deleteQuestionnaireImporterById :: String -> AppContextM Int64
deleteQuestionnaireImporterById qiId = do
  appUuid <- asks _appContextAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("id", qiId)]
