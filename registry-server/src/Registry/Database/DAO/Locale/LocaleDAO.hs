module Registry.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Database.DAO.Common
import Shared.Common.Util.String
import Shared.Locale.Database.Mapping.Locale.Locale ()
import Shared.Locale.Model.Locale.Locale

entityName = "locale"

findLocalesFiltered :: [(String, String)] -> Maybe String -> AppContextM [Locale]
findLocalesFiltered queryParams mRecommendedAppVersion = do
  tenantUuid <- asks (.tenantUuid')
  let queryParamCondition = mapToDBQuerySql (tenantQueryUuid tenantUuid : queryParams)
  let recommendedAppVersionCondition =
        case mRecommendedAppVersion of
          Just _ -> "AND (compare_version(recommended_app_version, ?) = 'LT' OR compare_version(recommended_app_version, ?) = 'EQ')"
          Nothing -> ""
  let sql = fromString $ f' "SELECT * FROM locale WHERE %s %s" [queryParamCondition, recommendedAppVersionCondition]
  let params = [U.toString tenantUuid] ++ fmap snd queryParams ++ maybeToList mRecommendedAppVersion ++ maybeToList mRecommendedAppVersion
  logQuery sql params
  let action conn = query conn sql params
  runDB action
