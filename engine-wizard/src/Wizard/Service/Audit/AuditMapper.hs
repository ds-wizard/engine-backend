module Wizard.Service.Audit.AuditMapper where

import Wizard.Model.Audit.Audit

toAudit uuid component action entity body createdBy appUuid createdAt =
  Audit
    { _auditUuid = uuid
    , _auditComponent = component
    , _auditAction = action
    , _auditEntity = entity
    , _auditBody = body
    , _auditCreatedBy = createdBy
    , _auditAppUuid = appUuid
    , _auditCreatedAt = createdAt
    }
