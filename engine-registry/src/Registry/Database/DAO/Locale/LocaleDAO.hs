module Registry.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Database.DAO.Common
import Shared.Database.Mapping.Locale.Locale ()
import Shared.Model.Locale.Locale
import Shared.Util.String

entityName = "locale"

findLocalesFiltered :: [(String, String)] -> Maybe String -> AppContextM [Locale]
findLocalesFiltered queryParams mRecommendedAppVersion = do
  appUuid <- asks (.appUuid')
  let queryParamCondition = mapToDBQuerySql (appQueryUuid appUuid : queryParams)
  let recommendedAppVersionCondition =
        case mRecommendedAppVersion of
          Just _ -> "AND compare_version(recommended_app_version, ?) = 'LT'"
          Nothing -> ""
  let sql = fromString $ f' "SELECT * FROM locale WHERE %s %s" [queryParamCondition, recommendedAppVersionCondition]
  let params = [U.toString appUuid] ++ fmap snd queryParams ++ maybeToList mRecommendedAppVersion
  logQuery sql params
  let action conn = query conn sql params
  runDB action
