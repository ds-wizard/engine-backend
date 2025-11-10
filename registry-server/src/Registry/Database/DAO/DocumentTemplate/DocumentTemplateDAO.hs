module Registry.Database.DAO.DocumentTemplate.DocumentTemplateDAO where

import Control.Monad.Reader (asks)
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Database.DAO.Common
import Shared.Common.Database.Mapping.Common.SemVer2Tuple ()
import Shared.Common.Model.Common.SemVer2Tuple
import Shared.Common.Util.String
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template"

findDocumentTemplatesFiltered :: [(String, String)] -> Maybe SemVer2Tuple -> AppContextM [DocumentTemplate]
findDocumentTemplatesFiltered queryParams mMetamodelVersion = do
  tenantUuid <- asks (.tenantUuid')
  let queryParamCondition = mapToDBQuerySql (tenantQueryUuid tenantUuid : queryParams)
  let (metamodelVersionCondition, metamodelVersionParam) =
        case mMetamodelVersion of
          Just metamodelVersion ->
            ( "AND ((metamodel_version).major < ? OR ((metamodel_version).major = ? AND (metamodel_version).minor <= ?))"
            , [toField metamodelVersion.major, toField metamodelVersion.major, toField metamodelVersion.minor]
            )
          Nothing -> ("", [])
  let sql = fromString $ f' "SELECT * FROM document_template WHERE %s %s" [queryParamCondition, metamodelVersionCondition]
  let params = toField tenantUuid : fmap (toField . snd) queryParams ++ metamodelVersionParam
  logQuery sql params
  let action conn = query conn sql params
  runDB action
