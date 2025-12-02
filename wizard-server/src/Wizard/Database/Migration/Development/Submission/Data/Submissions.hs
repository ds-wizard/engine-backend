module Wizard.Database.Migration.Development.Submission.Data.Submissions where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Submission.SubmissionCreateDTO
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Document.Document
import Wizard.Model.Submission.Submission
import Wizard.Model.Submission.SubmissionList
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Service.Submission.SubmissionMapper

submissionCreate :: SubmissionCreateDTO
submissionCreate = SubmissionCreateDTO {serviceId = defaultSubmissionService.sId}

submission1 :: Submission
submission1 =
  Submission
    { uuid = u' "f9e14cfb-4435-45a5-8a10-2ea5dbcb92b0"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = defaultSubmissionService.sId
    , documentUuid = doc1.uuid
    , tenantUuid = defaultTenant.uuid
    , createdBy = userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

submission1List :: SubmissionList
submission1List = toList submission1 defaultSubmissionService userAlbertSuggestion

submission2 :: Submission
submission2 =
  Submission
    { uuid = u' "bca23893-1d44-4980-a3ba-b50c6b8df342"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = defaultSubmissionService.sId
    , documentUuid = doc1.uuid
    , tenantUuid = defaultTenant.uuid
    , createdBy = userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

submission2Dto :: SubmissionList
submission2Dto = toList submission1 defaultSubmissionService userAlbertSuggestion

differentSubmission1 :: Submission
differentSubmission1 =
  Submission
    { uuid = u' "de51c280-2a6f-49d0-b3de-405401ffba74"
    , state = DoneSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = defaultSubmissionService.sId
    , documentUuid = differentDoc.uuid
    , tenantUuid = differentTenant.uuid
    , createdBy = userCharles.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
