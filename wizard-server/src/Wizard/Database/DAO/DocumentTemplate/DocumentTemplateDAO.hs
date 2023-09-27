module Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDAO where

import Control.Monad.Reader (asks, liftIO)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.String
import Wizard.Database.DAO.Common hiding (createCountGroupByCoordinateFn, createFindEntitiesGroupByCoordinatePageableQuerySortFn)
import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template"

pageLabel = "documentTemplates"

findDocumentTemplatesPage :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateList)
findDocumentTemplatesPage mOrganizationId mTemplateId mQuery mTemplateState mNonEditable pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let stateCondition =
          case mTemplateState of
            Just _ ->
              f'
                "AND get_template_state(registry_document_template.remote_version, document_template.version, %s, document_template.metamodel_version) = ?"
                [show documentTemplateMetamodelVersion]
            Nothing -> ""
    let nonEditableCondition =
          case mNonEditable of
            Just nonEditable -> f' "AND non_editable = '%s'" [show nonEditable]
            Nothing -> ""
    -- 2. Get total count
    count <- countDocumentTemplatesPage mQuery mOrganizationId mTemplateId mTemplateState stateCondition nonEditableCondition
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT document_template.*, get_template_state(registry_document_template.remote_version, document_template.version, %s, document_template.metamodel_version), registry_document_template.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo \
              \FROM document_template \
              \LEFT JOIN registry_document_template ON document_template.organization_id = registry_document_template.organization_id AND document_template.template_id = registry_document_template.template_id \
              \LEFT JOIN registry_organization ON document_template.organization_id = registry_organization.organization_id \
              \WHERE tenant_uuid = ? AND id IN ( \
              \    SELECT CONCAT(organization_id, ':', template_id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
              \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
              \                                                          (max(string_to_array(version, '.')::int[]))[3]) \
              \    FROM document_template \
              \    WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND tenant_uuid = ? AND (name ~* ? OR id ~* ?) %s \
              \    GROUP BY organization_id, template_id \
              \) \
              \%s \
              \%s \
              \%s \
              \offset %s \
              \limit %s"
              [ show documentTemplateMetamodelVersion
              , mapToDBCoordinatesSql entityName "template_id" mOrganizationId mTemplateId
              , stateCondition
              , nonEditableCondition
              , mapSort sort
              , show skip
              , show sizeI
              ]
    let params =
          U.toString tenantUuid
            : U.toString tenantUuid
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

countDocumentTemplatesPage :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> String -> String -> AppContextM Int
countDocumentTemplatesPage mQuery mOrganizationId mTemplateId mState stateCondition nonEditableCondition = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f'
            "SELECT count(*) \
            \FROM document_template \
            \LEFT JOIN registry_document_template ON document_template.organization_id = registry_document_template.organization_id AND document_template.template_id = registry_document_template.template_id \
            \WHERE tenant_uuid = ? AND (name ~* ? OR id ~* ?) %s %s %s \
            \  AND id IN (SELECT CONCAT(organization_id, ':', template_id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                                   (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                                   (max(string_to_array(version, '.')::int[]))[3]) \
            \             FROM document_template \
            \             WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND tenant_uuid = ? AND (name ~* ? OR id ~* ?) \
            \             GROUP BY organization_id, template_id)"
            [ mapToDBCoordinatesSql entityName "template_id" mOrganizationId mTemplateId
            , stateCondition
            , nonEditableCondition
            ]
  let params =
        U.toString tenantUuid
          : regex mQuery
          : regex mQuery
          : mapToDBCoordinatesParams mOrganizationId mTemplateId
          ++ maybeToList mState
          ++ [U.toString tenantUuid]
          ++ [regex mQuery, regex mQuery]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

findDocumentTemplatesFiltered :: [(String, String)] -> AppContextM [DocumentTemplate]
findDocumentTemplatesFiltered queryParams = do
  tenantUuid <- asks currentTenantUuid
  let queryCondition =
        case queryParams of
          [] -> ""
          _ -> f' "AND %s" [mapToDBQuerySql queryParams]
  let sql =
        fromString $
          f'
            "SELECT * \
            \FROM document_template \
            \WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') %s"
            [queryCondition]
  let params = U.toString tenantUuid : fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

touchDocumentTemplateById :: String -> AppContextM Int64
touchDocumentTemplateById tmlId = do
  tenantUuid <- asks currentTenantUuid
  now <- liftIO getCurrentTime
  let sql = fromString "UPDATE document_template SET updated_at = ? WHERE tenant_uuid = ? AND id = ?"
  let params = [toField now, toField tenantUuid, toField tmlId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
