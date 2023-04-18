module Wizard.Service.Audit.AuditMapper where

import Wizard.Model.Audit.Audit

toAudit uuid component action entity body createdBy appUuid createdAt =
  Audit
    { uuid = uuid
    , component = component
    , action = action
    , entity = entity
    , body = body
    , createdBy = createdBy
    , appUuid = appUuid
    , createdAt = createdAt
    }
