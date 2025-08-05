module Wizard.Service.Submission.SubmissionMapper where

import Data.Time
import qualified Data.UUID as U

import Wizard.Model.Submission.Submission
import Wizard.Model.Submission.SubmissionList
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

toList :: Submission -> TenantConfigSubmissionService -> UserSuggestionDTO -> SubmissionList
toList Submission {..} service createdBy2 =
  let serviceName = Just service.name
      createdBy = createdBy2
   in SubmissionList {..}

fromCreate :: U.UUID -> String -> U.UUID -> U.UUID -> U.UUID -> UTCTime -> Submission
fromCreate uuid serviceId documentUuid tenantUuid createdBy now =
  Submission
    { uuid = uuid
    , state = InProgressSubmissionState
    , location = Nothing
    , returnedData = Nothing
    , serviceId = serviceId
    , documentUuid = documentUuid
    , tenantUuid = tenantUuid
    , createdBy = createdBy
    , createdAt = now
    , updatedAt = now
    }
