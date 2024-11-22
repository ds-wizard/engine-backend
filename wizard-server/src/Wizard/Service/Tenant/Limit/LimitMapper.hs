module Wizard.Service.Tenant.Limit.LimitMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.Tenant.Limit.TenantLimitBundle
import WizardLib.Public.Model.Tenant.Limit.TenantLimitBundleChange

fromCreate :: U.UUID -> UTCTime -> TenantLimitBundle
fromCreate aUuid now =
  TenantLimitBundle
    { uuid = aUuid
    , users = -1000
    , activeUsers = -1000
    , knowledgeModels = -1000
    , branches = -1000
    , documentTemplates = -1000
    , documentTemplateDrafts = -1000
    , questionnaires = -1000
    , documents = -1000
    , locales = -1000
    , storage = -1000 * 5 * 1000 * 1000
    , createdAt = now
    , updatedAt = now
    }

fromChangeDTO :: TenantLimitBundle -> TenantLimitBundleChange -> UTCTime -> TenantLimitBundle
fromChangeDTO limitBundle reqDto now =
  TenantLimitBundle
    { uuid = limitBundle.uuid
    , users = reqDto.users
    , activeUsers = reqDto.activeUsers
    , knowledgeModels = reqDto.knowledgeModels
    , branches = reqDto.branches
    , documentTemplates = reqDto.documentTemplates
    , documentTemplateDrafts = reqDto.documentTemplateDrafts
    , questionnaires = reqDto.questionnaires
    , documents = reqDto.documents
    , locales = reqDto.locales
    , storage = reqDto.storage
    , createdAt = limitBundle.createdAt
    , updatedAt = now
    }
