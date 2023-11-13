module Shared.Audit.Service.Audit.AuditMapper where

import Shared.Audit.Model.Audit.Audit

toAudit uuid component action entity body createdBy tenantUuid createdAt =
  Audit
    { uuid = uuid
    , component = component
    , action = action
    , entity = entity
    , body = body
    , createdBy = createdBy
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    }
