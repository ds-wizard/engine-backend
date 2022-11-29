module Registry.Database.DAO.Package.PackageDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Database.DAO.Common
import Shared.Database.Mapping.Package.Package ()
import Shared.Model.Package.Package
import Shared.Util.String

entityName = "package"

findPackagesFiltered :: [(String, String)] -> Maybe Int -> AppContextM [Package]
findPackagesFiltered queryParams mMetamodelVersion = do
  appUuid <- asks (.appUuid')
  let queryParamCondition = mapToDBQuerySql (appQueryUuid appUuid : queryParams)
  let metamodelVersionCondition =
        case mMetamodelVersion of
          Just _ -> "AND metamodel_version <= ?"
          Nothing -> ""
  let sql = fromString $ f' "SELECT * FROM package WHERE %s %s" [queryParamCondition, metamodelVersionCondition]
  let params = [U.toString appUuid] ++ fmap snd queryParams ++ (fmap show . maybeToList $ mMetamodelVersion)
  logQuery sql params
  let action conn = query conn sql params
  runDB action
