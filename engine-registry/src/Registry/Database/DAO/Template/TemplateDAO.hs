module Registry.Database.DAO.Template.TemplateDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Database.DAO.Common
import Shared.Database.Mapping.Template.Template ()
import Shared.Model.Context.ContextLenses
import Shared.Model.Template.Template
import Shared.Util.String

entityName = "template"

findTemplatesFiltered :: [(String, String)] -> Maybe Int -> AppContextM [Template]
findTemplatesFiltered queryParams mMetamodelVersion = do
  appUuid <- asks (^. appUuid')
  let queryParamCondition = mapToDBQuerySql (appQueryUuid appUuid : queryParams)
  let metamodelVersionCondition =
        case mMetamodelVersion of
          Just _ -> "AND metamodel_version <= ?"
          Nothing -> ""
  let sql = fromString $ f' "SELECT * FROM template WHERE %s %s" [queryParamCondition, metamodelVersionCondition]
  let params = [U.toString appUuid] ++ fmap snd queryParams ++ (fmap show . maybeToList $ mMetamodelVersion)
  logQuery sql params
  let action conn = query conn sql params
  runDB action
