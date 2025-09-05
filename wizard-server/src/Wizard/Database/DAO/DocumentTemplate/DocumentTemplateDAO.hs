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
import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.DocumentTemplate.DocumentTemplateList
import Wizard.Model.DocumentTemplate.DocumentTemplateSuggestion
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template"

pageLabel = "documentTemplates"

findDocumentTemplatesPage :: Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page DocumentTemplateList)
findDocumentTemplatesPage mOrganizationId mTemplateId mQuery mOutdated mNonEditable pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let outdatedCondition =
          case mOutdated of
            Just _ -> "AND is_outdated(registry_document_template.remote_version, document_template.version) = ?"
            Nothing -> ""
    let nonEditableCondition =
          case mNonEditable of
            Just nonEditable -> f' "AND non_editable = '%s'" [show nonEditable]
            Nothing -> ""
    -- 2. Get total count
    count <- countDocumentTemplatesPage mQuery mOrganizationId mTemplateId mOutdated outdatedCondition nonEditableCondition
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT \
              \   document_template.id, \
              \   document_template.name, \
              \   document_template.organization_id, \
              \   document_template.template_id, \
              \   document_template.version, \
              \   document_template.phase, \
              \   document_template.metamodel_version, \
              \   document_template.description, \
              \   document_template.allowed_packages, \
              \   document_template.non_editable, \
              \   registry_document_template.remote_version, \
              \   registry_organization.name as org_name, \
              \   registry_organization.logo as org_logo, \
              \   document_template.created_at \
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
              \OFFSET %s \
              \LIMIT %s"
              [ mapToDBCoordinatesSql entityName "template_id" mOrganizationId mTemplateId
              , outdatedCondition
              , nonEditableCondition
              , mapSort sort
              , show skip
              , show sizeI
              ]
    let params =
          toField tenantUuid
            : toField tenantUuid
            : toField (regexM mQuery)
            : toField (regexM mQuery)
            : fmap toField (mapToDBCoordinatesParams mOrganizationId mTemplateId)
            ++ (maybeToList . fmap toField $ mOutdated)
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

findDocumentTemplatesSuggestions :: Maybe String -> Maybe Bool -> AppContextM [DocumentTemplateSuggestion]
findDocumentTemplatesSuggestions mQuery mNonEditable = do
  tenantUuid <- asks currentTenantUuid
  let nonEditableCondition =
        case mNonEditable of
          Just nonEditable -> f' "AND non_editable = '%s'" [show nonEditable]
          Nothing -> ""
  let sql =
        fromString $
          f'
            "SELECT \
            \   document_template.id, \
            \   document_template.name, \
            \   document_template.organization_id, \
            \   document_template.template_id, \
            \   document_template.version, \
            \   document_template.phase, \
            \   document_template.metamodel_version, \
            \   document_template.description, \
            \   document_template.allowed_packages, \
            \   ( \
            \    SELECT coalesce(jsonb_agg(jsonb_build_object('uuid', uuid, 'name', name, 'icon', icon)), '[]'::jsonb) \
            \    FROM (SELECT * \
            \          FROM document_template_format dt_format \
            \          WHERE dt_format.tenant_uuid = document_template.tenant_uuid \
            \            AND dt_format.document_template_id = document_template.id \
            \          ORDER BY dt_format.name) nested \
            \   ) AS document_template_formats \
            \FROM document_template \
            \WHERE tenant_uuid = ? AND id IN ( \
            \    SELECT CONCAT(organization_id, ':', template_id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[3]) \
            \    FROM document_template \
            \    WHERE (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') AND tenant_uuid = ? AND (name ~* ? OR id ~* ?) \
            \    GROUP BY organization_id, template_id \
            \) \
            \%s"
            [nonEditableCondition]
  let params = [U.toString tenantUuid, U.toString tenantUuid, regexM mQuery, regexM mQuery]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

countDocumentTemplatesPage :: Maybe String -> Maybe String -> Maybe String -> Maybe Bool -> String -> String -> AppContextM Int
countDocumentTemplatesPage mQuery mOrganizationId mTemplateId mOutdated outdatedCondition nonEditableCondition = do
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
            , outdatedCondition
            , nonEditableCondition
            ]
  let params =
        toField tenantUuid
          : toField (regexM mQuery)
          : toField (regexM mQuery)
          : fmap toField (mapToDBCoordinatesParams mOrganizationId mTemplateId)
          ++ (maybeToList . fmap toField $ mOutdated)
          ++ [toField tenantUuid, toField $ regexM mQuery, toField $ regexM mQuery]
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
