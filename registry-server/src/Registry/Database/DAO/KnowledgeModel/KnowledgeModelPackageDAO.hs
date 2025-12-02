module Registry.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Database.DAO.Common
import Shared.Common.Util.String
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackage ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

entityName = "knowledge_model_package"

findPackagesFiltered :: [(String, String)] -> Maybe Int -> AppContextM [KnowledgeModelPackage]
findPackagesFiltered queryParams mMetamodelVersion = do
  tenantUuid <- asks (.tenantUuid')
  let queryParamCondition = mapToDBQuerySql (tenantQueryUuid tenantUuid : queryParams)
  let metamodelVersionCondition =
        case mMetamodelVersion of
          Just _ -> "AND metamodel_version <= ?"
          Nothing -> ""
  let sql = fromString $ f' "SELECT * FROM knowledge_model_package WHERE %s %s" [queryParamCondition, metamodelVersionCondition]
  let params = [U.toString tenantUuid] ++ fmap snd queryParams ++ (fmap show . maybeToList $ mMetamodelVersion)
  logQuery sql params
  let action conn = query conn sql params
  runDB action
