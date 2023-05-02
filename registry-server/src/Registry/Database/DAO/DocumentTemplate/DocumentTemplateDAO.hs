module Registry.Database.DAO.DocumentTemplate.DocumentTemplateDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Database.DAO.Common
import Shared.Common.Util.String
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template"

findDocumentTemplatesFiltered :: [(String, String)] -> Maybe Int -> AppContextM [DocumentTemplate]
findDocumentTemplatesFiltered queryParams mMetamodelVersion = do
  appUuid <- asks (.appUuid')
  let queryParamCondition = mapToDBQuerySql (appQueryUuid appUuid : queryParams)
  let metamodelVersionCondition =
        case mMetamodelVersion of
          Just _ -> "AND metamodel_version <= ?"
          Nothing -> ""
  let sql = fromString $ f' "SELECT * FROM document_template WHERE %s %s" [queryParamCondition, metamodelVersionCondition]
  let params = [U.toString appUuid] ++ fmap snd queryParams ++ (fmap show . maybeToList $ mMetamodelVersion)
  logQuery sql params
  let action conn = query conn sql params
  runDB action
