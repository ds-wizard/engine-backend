module Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Tenant.Config.TenantConfigSubmission ()
import Wizard.Database.Mapping.Tenant.Config.TenantConfigSubmissionServiceSimple ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Config.TenantConfigSubmissionServiceSimple

-- --------------------------------------------------------
-- SUBMISSION
-- --------------------------------------------------------
findTenantConfigSubmission :: AppContextM TenantConfigSubmission
findTenantConfigSubmission = do
  tenantUuid <- asks currentTenantUuid
  findTenantConfigSubmissionByUuid tenantUuid

findTenantConfigSubmissionByUuid :: U.UUID -> AppContextM TenantConfigSubmission
findTenantConfigSubmissionByUuid tenantUuid = do
  tcSubmission <- createFindEntityByFn "config_submission" [("tenant_uuid", U.toString tenantUuid)]
  tcSubmissionServices <- findTenantConfigSubmissionServices
  return $ tcSubmission {services = tcSubmissionServices}

insertTenantConfigSubmission :: TenantConfigSubmission -> AppContextM ()
insertTenantConfigSubmission submission = do
  createInsertFn "config_submission" submission
  traverse_ insertOrUpdateConfigSubmissionService submission.services

updateTenantConfigSubmission :: TenantConfigSubmission -> AppContextM Int64
updateTenantConfigSubmission submission = do
  let sql = "UPDATE config_submission SET tenant_uuid = ?, enabled = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?;"
  let params = toRow submission ++ [toField submission.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  -- Update services
  traverse_ insertOrUpdateConfigSubmissionService submission.services
  deleteTenantConfigSubmissionsExcept (fmap (.sId) submission.services)

deleteTenantConfigSubmissions :: AppContextM Int64
deleteTenantConfigSubmissions = createDeleteEntitiesFn "config_submission"

-- --------------------------------------------------------
-- SUBMISSION SERVICE
-- --------------------------------------------------------
findTenantConfigSubmissionServices :: AppContextM [TenantConfigSubmissionService]
findTenantConfigSubmissionServices = do
  tenantUuid <- asks currentTenantUuid
  tcSubmissionServices <- createFindEntitiesByFn "config_submission_service" [("tenant_uuid", U.toString tenantUuid)]
  traverse
    ( \service -> do
        supportedFormats <- findTenantConfigSubmissionServiceSupportedFormats service.sId
        requestHeaders <- findTenantConfigSubmissionServiceRequestHeaders service.sId
        return $
          service
            { supportedFormats = supportedFormats
            , request = service.request {headers = requestHeaders}
            }
    )
    tcSubmissionServices

findTenantConfigSubmissionServicesByDocumentTemplateIdAndFormatUuid :: String -> U.UUID -> AppContextM [TenantConfigSubmissionServiceSimple]
findTenantConfigSubmissionServicesByDocumentTemplateIdAndFormatUuid documentTemplateId formatUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT DISTINCT service.id, \
          \                service.name, \
          \                service.description \
          \FROM config_submission_service service \
          \     JOIN config_submission_service_supported_format supported_format \
          \          ON service.tenant_uuid = supported_format.tenant_uuid AND \
          \             service.id = supported_format.service_id \
          \WHERE service.tenant_uuid = ? \
          \  AND supported_format.document_template_id = ? \
          \  AND supported_format.format_uuid = ? \
          \ORDER BY service.id;"
  let params = [toField tenantUuid, toField documentTemplateId, toField formatUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findTenantConfigSubmissionServiceByServiceId :: String -> AppContextM TenantConfigSubmissionService
findTenantConfigSubmissionServiceByServiceId serviceId = do
  tenantUuid <- asks currentTenantUuid
  service <- createFindEntityByFn "config_submission_service" [("tenant_uuid", U.toString tenantUuid), ("id", serviceId)]
  supportedFormats <- findTenantConfigSubmissionServiceSupportedFormats service.sId
  requestHeaders <- findTenantConfigSubmissionServiceRequestHeaders service.sId
  return $
    service
      { supportedFormats = supportedFormats
      , request = service.request {headers = requestHeaders}
      }

insertOrUpdateConfigSubmissionService :: TenantConfigSubmissionService -> AppContextM Int64
insertOrUpdateConfigSubmissionService service = do
  let sql =
        fromString
          "INSERT INTO config_submission_service \
          \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
          \ON CONFLICT (tenant_uuid, id) DO UPDATE SET tenant_uuid                 = ?, \
          \                                            id                          = ?, \
          \                                            name                        = ?, \
          \                                            description                 = ?, \
          \                                            props                       = ?, \
          \                                            request_method              = ?, \
          \                                            request_url                 = ?, \
          \                                            request_multipart_enabled   = ?, \
          \                                            request_multipart_file_name = ?, \
          \                                            created_at                  = ?, \
          \                                            updated_at                  = ?;"
  let params = toRow service ++ toRow service
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  -- Update request headers
  traverse_ (insertOrUpdateConfigSubmissionServiceRequestHeader service.tenantUuid service.sId) (M.toList service.request.headers)
  deleteTenantConfigSubmissionsRequestHeaderExcept service.sId (M.keys service.request.headers)
  -- Update supported formats
  traverse_ insertOrUpdateConfigSubmissionServiceSupportedFormat service.supportedFormats
  deleteTenantConfigSubmissionsSupportedFormatExcept service.sId service.supportedFormats

deleteTenantConfigSubmissionsExcept :: [String] -> AppContextM Int64
deleteTenantConfigSubmissionsExcept serviceIds = do
  let serviceIdCondition =
        case serviceIds of
          [] -> ""
          _ -> f' "AND id NOT IN (%s)" [generateQuestionMarks serviceIds]
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f'
            "DELETE FROM config_submission_service \
            \WHERE tenant_uuid = ? %s"
            [serviceIdCondition]
  let params = U.toString tenantUuid : serviceIds
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

-- --------------------------------------------------------
-- SUBMISSION SERVICE SUPPORTED FORMAT
-- --------------------------------------------------------
findTenantConfigSubmissionServiceSupportedFormats :: String -> AppContextM [TenantConfigSubmissionServiceSupportedFormat]
findTenantConfigSubmissionServiceSupportedFormats serviceId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn "config_submission_service_supported_format" [("tenant_uuid", U.toString tenantUuid), ("service_id", serviceId)]

insertOrUpdateConfigSubmissionServiceSupportedFormat :: TenantConfigSubmissionServiceSupportedFormat -> AppContextM Int64
insertOrUpdateConfigSubmissionServiceSupportedFormat supportedFormat = do
  let sql =
        fromString
          "INSERT INTO config_submission_service_supported_format \
          \VALUES (?, ?, ?, ?) \
          \ON CONFLICT (tenant_uuid, service_id, document_template_id, format_uuid) DO NOTHING"
  let params = toRow supportedFormat
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigSubmissionsSupportedFormatExcept :: String -> [TenantConfigSubmissionServiceSupportedFormat] -> AppContextM Int64
deleteTenantConfigSubmissionsSupportedFormatExcept serviceId supportedFormats = do
  tenantUuid <- asks currentTenantUuid
  let condition =
        case supportedFormats of
          [] -> ""
          _ ->
            f'
              "AND NOT (%s)"
              [L.intercalate " OR " (replicate (length supportedFormats) "(document_template_id = ? AND format_uuid = ?)")]
  let sql = fromString $ f' "DELETE FROM config_submission_service_supported_format WHERE tenant_uuid = ? AND service_id = ? %s" [condition]
  let params = [toField tenantUuid, toField serviceId] ++ concatMap (\supportedFormat -> [toField supportedFormat.templateId, toField supportedFormat.formatUuid]) supportedFormats
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

-- --------------------------------------------------------
-- SUBMISSION SERVICE REQUEST HEADER
-- --------------------------------------------------------
findTenantConfigSubmissionServiceRequestHeaders :: String -> AppContextM (M.Map String String)
findTenantConfigSubmissionServiceRequestHeaders serviceId = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT name, value \
          \FROM config_submission_service_request_header \
          \WHERE tenant_uuid = ? AND service_id = ? \
          \GROUP BY name, value"
  let params = [toField tenantUuid, toField serviceId]
  logQuery sql params
  let action conn = query conn sql params
  results <- runDB action
  return . M.fromList $ results

insertOrUpdateConfigSubmissionServiceRequestHeader :: U.UUID -> String -> (String, String) -> AppContextM Int64
insertOrUpdateConfigSubmissionServiceRequestHeader tenantUuid serviceId (name, value) = do
  let sql =
        fromString
          "INSERT INTO config_submission_service_request_header \
          \VALUES (?, ?, ?, ?) \
          \ON CONFLICT (tenant_uuid, service_id, name) DO UPDATE SET value = ?"
  let params = [U.toString tenantUuid, serviceId, name, value, value]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigSubmissionsRequestHeaderExcept :: String -> [String] -> AppContextM Int64
deleteTenantConfigSubmissionsRequestHeaderExcept serviceId names = do
  tenantUuid <- asks currentTenantUuid
  let condition =
        case names of
          [] -> ""
          _ ->
            f'
              "AND NOT (%s)"
              [L.intercalate " OR " (replicate (length names) "name = ?")]
  let sql = fromString $ f' "DELETE FROM config_submission_service_request_header WHERE tenant_uuid = ? AND service_id = ? %s" [condition]
  let params = [toField tenantUuid, toField serviceId] ++ fmap toField names
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
