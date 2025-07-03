module Wizard.Database.Mapping.Tenant.Config.TenantConfigSubmission where

import qualified Data.Map.Strict as M
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Wizard.Model.Tenant.Config.TenantConfig

instance ToRow TenantConfigSubmission where
  toRow TenantConfigSubmission {..} =
    [ toField tenantUuid
    , toField enabled
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow TenantConfigSubmission where
  fromRow = do
    tenantUuid <- field
    enabled <- field
    let services = []
    createdAt <- field
    updatedAt <- field
    return $ TenantConfigSubmission {..}

instance ToRow TenantConfigSubmissionService where
  toRow TenantConfigSubmissionService {..} =
    [ toField tenantUuid
    , toField sId
    , toField name
    , toField description
    , toField . PGArray $ props
    , toField request.method
    , toField request.url
    , toField request.multipart.enabled
    , toField request.multipart.fileName
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow TenantConfigSubmissionService where
  fromRow = do
    tenantUuid <- field
    sId <- field
    name <- field
    description <- field
    props <- fromPGArray <$> field
    let supportedFormats = []
    requestMethod <- field
    requestUrl <- field
    requestMultipartEnabled <- field
    requestMultipartFileName <- field
    let request =
          TenantConfigSubmissionServiceRequest
            { method = requestMethod
            , url = requestUrl
            , headers = M.empty
            , multipart =
                TenantConfigSubmissionServiceRequestMultipart
                  { enabled = requestMultipartEnabled
                  , fileName = requestMultipartFileName
                  }
            }
    createdAt <- field
    updatedAt <- field
    return $ TenantConfigSubmissionService {..}

instance ToRow TenantConfigSubmissionServiceSupportedFormat

instance FromRow TenantConfigSubmissionServiceSupportedFormat
