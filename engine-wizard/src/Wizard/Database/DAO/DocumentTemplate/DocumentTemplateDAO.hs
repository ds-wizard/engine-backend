module Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Shared.Constant.DocumentTemplate
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.String
import Wizard.Database.DAO.Common hiding (createCountGroupByCoordinateFn, createFindEntitiesGroupByCoordinatePageableQuerySortFn)
import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.DocumentTemplate.DocumentTemplateList

entityName = "document_template"

pageLabel = "documentTemplates"

findDocumentTemplatesPage :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateList)
findDocumentTemplatesPage mOrganizationId mTemplateId mQuery mTemplateState pageable sort =
  -- 1. Prepare variables
  do
    appUuid <- asks currentAppUuid
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let stateCondition =
          case mTemplateState of
            Just _ ->
              f'
                "AND get_template_state(registry_document_template.remote_version, document_template.version, %s, document_template.metamodel_version) = ?"
                [show documentTemplateMetamodelVersion]
            Nothing -> ""
    -- 2. Get total count
    count <- countDocumentTemplatesPage mQuery mOrganizationId mTemplateId mTemplateState stateCondition
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT document_template.*, get_template_state(registry_document_template.remote_version, document_template.version, %s, document_template.metamodel_version), registry_document_template.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo \
              \FROM document_template \
              \LEFT JOIN registry_document_template ON document_template.organization_id = registry_document_template.organization_id AND document_template.template_id = registry_document_template.template_id \
              \LEFT JOIN registry_organization ON document_template.organization_id = registry_organization.organization_id \
              \WHERE app_uuid = ? AND id IN ( \
              \    SELECT CONCAT(organization_id, ':', template_id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
              \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
              \                                                          (max(string_to_array(version, '.')::int[]))[3]) \
              \    FROM document_template \
              \    WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND app_uuid = ? AND (name ~* ? OR id ~* ?) %s \
              \    GROUP BY organization_id, template_id \
              \) \
              \%s \
              \%s \
              \offset %s \
              \limit %s"
              [ show documentTemplateMetamodelVersion
              , mapToDBCoordinatesSql entityName "template_id" mOrganizationId mTemplateId
              , stateCondition
              , mapSort sort
              , show skip
              , show sizeI
              ]
    let params =
          U.toString appUuid
            : U.toString appUuid
            : regex mQuery
            : regex mQuery
            : mapToDBCoordinatesParams mOrganizationId mTemplateId
            ++ maybeToList mTemplateState
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 4. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

countDocumentTemplatesPage :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> String -> AppContextM Int
countDocumentTemplatesPage mQuery mOrganizationId mTemplateId mState stateCondition = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString $
          f'
            "SELECT count(*) \
            \FROM document_template \
            \LEFT JOIN registry_document_template ON document_template.organization_id = registry_document_template.organization_id AND document_template.template_id = registry_document_template.template_id \
            \WHERE app_uuid = ? AND (name ~* ? OR id ~* ?) %s %s \
            \  AND id IN (SELECT CONCAT(organization_id, ':', template_id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                                   (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                                   (max(string_to_array(version, '.')::int[]))[3]) \
            \             FROM document_template \
            \             WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND app_uuid = ? AND (name ~* ? OR id ~* ?) \
            \             GROUP BY organization_id, template_id)"
            [ mapToDBCoordinatesSql entityName "template_id" mOrganizationId mTemplateId
            , stateCondition
            ]
  let params =
        U.toString appUuid
          : regex mQuery
          : regex mQuery
          : mapToDBCoordinatesParams mOrganizationId mTemplateId
          ++ maybeToList mState
          ++ [U.toString appUuid]
          ++ [regex mQuery, regex mQuery]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

findDocumentTemplatesFiltered :: [(String, String)] -> AppContextM [DocumentTemplate]
findDocumentTemplatesFiltered queryParams = do
  appUuid <- asks currentAppUuid
  let queryCondition =
        case queryParams of
          [] -> ""
          _ -> f' "AND %s" [mapToDBQuerySql queryParams]
  let sql =
        fromString $
          f'
            "SELECT * \
            \FROM document_template \
            \WHERE app_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') %s"
            [queryCondition]
  let params = U.toString appUuid : fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action
